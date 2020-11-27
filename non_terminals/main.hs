module MainGrammar where

import Lexer
import Text.Parsec

import FlowPrimTokens
import LiteralsPrimTokens
import MainPrimTokens
import TypesPrimTokens
import OperatorsPrimTokens
import ScopesPrimTokens

import ExprGrammar
import AssignmentGrammar
import DeclrGrammar
import VoidCommandsGrammar
import SubProgGrammar

import MemTable
import SubProgTable
import TypesTable
import OurState

import ExecutionUtils


--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------


-- <program> -> { <import> } <blocks> // IMPORTS EH OPICIONAL
programParser :: ParsecT [Token] OurState IO([Token])
programParser = (do   imports <- many importParser
                      blocks <- blocksParser
                      eof
                      return (concat(imports) ++ blocks))


-- <import> -> HASHTAG IMPORT STRING_LIT {SEPARATOR}
importParser :: ParsecT [Token] OurState IO([Token])
importParser = (do  hashtag <- hashtagToken
                    importt <- importToken
                    fileName <- stringLitToken
                    sep <- many separatorToken
                    return (hashtag:importt:[fileName]))


--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------


-- <blocks> -> { <block> }
blocksParser :: ParsecT [Token] OurState IO([Token])
blocksParser = (do  blocks <- many blockParser
                    return (concat(blocks)))


-- <block> -> <stmt> SEPARATOR | <enclosed_blocks>
blockParser :: ParsecT [Token] OurState IO([Token])
blockParser = (do   stmt <- stmtParser
                    sep <- separatorToken
                    return (stmt ++ [sep])) <|>
              (do   updateState (addToScope "")
                    enclosedBlocks <- enclosedBlocksParser
                    updateState removeFromScope
                    return enclosedBlocks)


-- <enclosed_blocks> -> LEFT_BRACE <blocks> RIGHT_BRACE
enclosedBlocksParser :: ParsecT [Token] OurState IO([Token])
enclosedBlocksParser = (do  leftBrace <- leftBraceToken
                            blocks <- blocksParser
                            rightBrace <- rightBraceToken
                            return (leftBrace:blocks ++ [rightBrace]))


--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------


-- <stmt> -> CONTINUE | BREAK | <return> | <void_command> | <compound_stmt> | <declrs> | <deref_pointer> <assignments_op> | <stmt_id>
stmtParser :: ParsecT [Token] OurState IO([Token])
stmtParser = (do  x <- continueToken <|> breakToken
                  return [x]) <|>
             (do  x <- returnParser <|> voidCommandParser <|> compoundStmtParser <|> declrsParser <|> stmtIdParser
                  return x) <|>
             (do  derefPointer <- derefPointerParser
                  assignmentsOp <- assignmentsOpParser derefPointer
                  return (derefPointer ++ assignmentsOp))


-- <stmt_id> -> ID ([<index_op>] <assignments_op> | <funcall_op>)
stmtIdParser :: ParsecT [Token] OurState IO([Token])
stmtIdParser = (do  idd <- idToken
                    assignmentOrFuncall <- (do  maybeIndex <- indexOpParser <|> (return [])
                                                assignment <- assignmentsOpParser (idd:maybeIndex)
                                                return (maybeIndex ++ assignment)) <|>
                                            funcallOpParser
                    return (idd:assignmentOrFuncall))





-- <compound_stmt> -> <control_structures> | <subprograms> | <struct>
compoundStmtParser :: ParsecT [Token] OurState IO([Token])
compoundStmtParser = (do  x <- controlStructureParser <|> subprogramsParser <|> structParser
                          return (x))


--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

-- <struct> -> STRUCT TYPE_ID LEFT_BRACE { <declrs> }+ RIGHT_BRACE
structParser :: ParsecT [Token] OurState IO([Token])
structParser = (do  struct <- structToken
                    idd <- typeIdToken
                    leftBrace <- leftBraceToken
                    -- updateState(typesTable INSERT (getStruct idd declrs))
                    declrs <- many1 (do x <- declrsParser
                                        y <- separatorToken
                                        return (x ++ [y]))
                    rightBrace <- rightBraceToken
                    return (struct:idd:leftBrace:concat(declrs) ++ [rightBrace]))


--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
{-
Os subprogramas serao DECLARADOS com a flag EM_EXEC DESATIVADA
e serao EXECUTADOS com a flag EM_EXEC ATIVADA

Na declaracao, o bloco de tokens do subprograma tambem sera salvo na tabela de funcoes
para futura execucao:
  Quando um subpr for declarado, serao salvos:
    seu protocolo
    seu nome
    seu retorno (se presente)
    seus comandos (bloco de execucao)
      esse como uma lista de Tokens
-}

-- <subprograms> -> <func> | <proc>
subprogramsParser :: ParsecT [Token] OurState IO([Token])
subprogramsParser = (do   x <- funcParser <|> procParser
                          return x)


-- <func> -> FUNC <type> ID <enclosed_args> <enclosed_blocks>
funcParser :: ParsecT [Token] OurState IO([Token])
funcParser =  (do   func <- funcToken
                    (_, typee) <- typeParser
                    idd <- idToken
                    enclosedArgs <- enclosedArgsParser
                    enclosedBlocks <- enclosedBlocksParser
                    return (func:typee ++ idd:enclosedArgs ++ enclosedBlocks))


-- <proc> -> PROC ID <enclosed_args> <enclosed_blocks>
procParser :: ParsecT [Token] OurState IO([Token])
procParser =  (do   procc <- procToken
                    idd <- idToken
                    enclosedArgs <- enclosedArgsParser
                    enclosedBlocks <- enclosedBlocksParser
                    return (procc:idd:enclosedArgs ++ enclosedBlocks))


--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------


-- <control_structures> -> <if> | <while> | <for>
controlStructureParser :: ParsecT [Token] OurState IO([Token])
controlStructureParser =  (do   x <- whileParser <|> forParser <|> ifParser
                                return (x))



-- <while> -> WHILE <enclosed_expr> <enclosed_blocks>
whileParser :: ParsecT [Token] OurState IO([Token])
whileParser =   (do   while <- whileToken
                      (_, enclosedExpr) <- enclosedExprParser
                      enclosedBlock <- enclosedBlocksParser
                      return (while:enclosedExpr ++ enclosedBlock))


-- <for> -> FOR LEFT_PAREN [ ( <declr> | <assignment> ) ] SEMICOLON [ <expr> ] SEMICOLON [ ( <declr> | <assignment> ) ] RIGHT_PAREN <enclosed_blocks>
forParser :: ParsecT [Token] OurState IO([Token])
forParser =   (do   for <- forToken
                    leftParen <- leftParenToken
                    maybeVarBinding1 <- assignmentsParser <|> declrsParser <|> (return [])
                    semicolon <- separatorToken
                    maybeExpr <- (do  (_, x) <- exprParser
                                      return x) <|>
                                (return [])
                    semicolon <- separatorToken
                    maybeVarBinding2 <- assignmentsParser <|> declrsParser <|> (return [])
                    rightParen <- rightParenToken
                    enclosedBlock <- enclosedBlocksParser
                    return (for:leftParen:maybeVarBinding1 ++ semicolon:maybeExpr ++ semicolon:maybeVarBinding2 ++ rightParen:enclosedBlock))


{-
O problema: O IF pode ter um ELSE

Se a flag de execução estiver ativada
  Ou o IF ou o ELSE serao executados
    Se a condicao do IF for verdadeira, os blocos devem ser executados
    Caso contrario, os blocos do ELSE deverao ser executados

Se entrar no IF com exec OFF, nada acontece
Se entrar no IF com exec ON,
  OU O bloco do IF acontece
  OU O bloco do ELSE acontece


-}

-- <if> -> IF <enclosed_expr> THEN <enclosed_blocks> [ ELSE ( <if> | <enclosed_blocks> ) ]
ifParser :: ParsecT [Token] OurState IO([Token])
ifParser =  (do   iff <- ifToken
                  (exprValue, enclosedExpr) <- enclosedExprParser
                  thenn <- thenToken
                  s <- getState
                  updateState (addToScope "if")
                  if (not (isExecOn s)) then do
                    enclosedBlock <- enclosedBlocksParser
                    updateState removeFromScope
                    maybeElse <- maybeElseParser
                    return (iff:enclosedExpr ++ thenn:enclosedBlock ++ maybeElse)
                  else do
                    if getBoolValue exprValue then do
                      enclosedBlock <- enclosedBlocksParser
                      updateState removeFromScope
                      updateState turnExecOff
                      maybeElse <- maybeElseParser
                      updateState turnExecOn
                      return (iff:enclosedExpr ++ thenn:enclosedBlock ++ maybeElse)
                    else do
                      updateState turnExecOff
                      enclosedBlock <- enclosedBlocksParser
                      updateState removeFromScope
                      updateState turnExecOn
                      maybeElse <- maybeElseParser
                      return (iff:enclosedExpr ++ thenn:enclosedBlock ++ maybeElse))


maybeElseParser :: ParsecT [Token] OurState IO([Token])
maybeElseParser = (do   updateState (addToScope "if")
                        elsee <- elseToken
                        ifOrEnclosed <- ifParser <|> enclosedBlocksParser
                        updateState removeFromScope
                        return (elsee:ifOrEnclosed)) <|>
                  (return [])
