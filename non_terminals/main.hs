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
import OurType

import BasicExecUtils


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
                    sep <- separatorToken
                    return (hashtag:importt:fileName:[sep]))


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
              (do   compoundStmt <- controlStructureParser <|> subprogramsParser <|> structParser
                    return (compoundStmt)) <|>
              (do   updateState (addToScope "") -- entrando num novo escopo
                    enclosedBlocks <- enclosedBlocksParser
                    updateState removeFromScope -- saindo do escopo criado
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


-- <stmt> -> CONTINUE | BREAK | <return> | <void_command> | <declrs> | <deref_pointer> <assignments_op> | <stmt_id>
stmtParser :: ParsecT [Token] OurState IO([Token])
stmtParser = (do  x <- continueToken <|> breakToken
                  return [x]) <|>
             (do  x <- returnParser <|> voidCommandParser <|> stmtIdParser
                  return x) <|>
             (do  derefPointer <- derefPointerParser
                  assignmentsOp <- assignmentsOpParser derefPointer
                  return (derefPointer ++ assignmentsOp)) <|>
             (do  (_, declrs) <- declrsParser
                  return (declrs))


-- <stmt_id> -> ID ([<index_op>] <assignments_op> | <funcall_op>)
stmtIdParser :: ParsecT [Token] OurState IO([Token])
stmtIdParser = (do  idd <- idToken
                    assignmentOrFuncall <- (do  maybeIndex <- indexOpParser <|> (return [])
                                                assignment <- assignmentsOpParser (idd:maybeIndex)
                                                return (maybeIndex ++ assignment)) <|>
                                            funcallOpParser
                    return (idd:assignmentOrFuncall))


--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------


-- <struct> -> STRUCT TYPE_ID LEFT_BRACE { <declrs> }+ RIGHT_BRACE
structParser :: ParsecT [Token] OurState IO([Token])
structParser = (do  struct <- structToken
                    idd <- typeIdToken
                    leftBrace <- leftBraceToken
                    s <- getState
                    -- se exec tiver on, vou desliga-lo pra processar as declrs
                    -- e liga-lo novamente. entao, insiro a struct criada na tabela
                    -- de tipos. se exec tiver off (vai estar off? kk) so processo os
                    -- tokens
                    if (isExecOn s) then do
                      updateState turnExecOff
                      (declrs, declrsTokens) <- multipleDeclrsParser
                      updateState turnExecOn
                      rightBrace <- rightBraceToken
                      updateState (typesTable INSERT (StructType (getStringFromId idd, declrs)))
                      return (struct:idd:leftBrace:declrsTokens ++ [rightBrace])
                    else do
                      (declrs, declrsTokens) <- multipleDeclrsParser
                      rightBrace <- rightBraceToken
                      return (struct:idd:leftBrace:declrsTokens ++ [rightBrace]))


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
                    (semanType, typee) <- typeParser
                    idd <- idToken
                    leftParen <- leftParenToken
                    (argsSeman, args) <- argsParser
                    rightParen <- rightParenToken
                    enclosedBlocks <- enclosedBlocksParser

                    updateState (funcTable INSERT (getStringFromId idd, semanType, argsSeman, args ++ enclosedBlocks))

                    return (func:typee ++ idd:leftParen:args ++ rightParen:enclosedBlocks))


-- <proc> -> PROC ID <enclosed_args> <enclosed_blocks>
procParser :: ParsecT [Token] OurState IO([Token])
procParser =  (do   procc <- procToken
                    idd <- idToken
                    leftParen <- leftParenToken
                    (argsSeman, args) <- argsParser
                    rightParen <- rightParenToken
                    enclosedBlocks <- enclosedBlocksParser

                    updateState (procTable INSERT (getStringFromId idd, argsSeman, args ++ enclosedBlocks))

                    return (procc:idd:leftParen:args ++ rightParen:enclosedBlocks))


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
                    maybeVarBinding1 <- (do   (_, declrs) <- declrsParser
                                              return (declrs)) <|>
                                              assignmentsParser <|>
                                              (return [])
                    semicolon <- separatorToken
                    maybeExpr <- (do  (_, x) <- exprParser
                                      return x) <|>
                                (return [])
                    semicolon <- separatorToken
                    maybeVarBinding2 <- (do   (_, declrs) <- declrsParser
                                              return (declrs)) <|>
                                              assignmentsParser <|>
                                              (return [])
                    rightParen <- rightParenToken
                    enclosedBlock <- enclosedBlocksParser
                    return (for:leftParen:maybeVarBinding1 ++ semicolon:maybeExpr ++ semicolon:maybeVarBinding2 ++ rightParen:enclosedBlock))


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
