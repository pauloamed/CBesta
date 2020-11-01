module MainGrammar where

import Lexer
import Text.Parsec

import FlowPrimTokens
import LiteralsPrimTokens
import MainPrimTokens
import OperatorsPrimTokens
import ScopesPrimTokens


-- <program> -> { <import> } <blocks> // IMPORTS EH OPICIONAL
programParser :: Parsec [Token] st [Token]
programParser = (do   imports <- many import
                      blocks <- blocksParser
                      return (imports ++ blocks))


-- <import> -> HASHTAG IMPORT STRING_LIT
importParser :: Parsec [Token] st [Token]
importParser = (do  hashtag <- hashtagToken
                    importt <- importToken
                    fileName <- stringLitToken
                    return (hashtag:importt:[fileName]))

--
-- <blocks> -> { <block> }+
blocksParser :: Parsec [Token] st [Token]
blocksParser = (do  blocks <- many1 blockParser
                    return concat(blocks))


-- <block> -> <stmt> SEPARATOR | <enclosed_blocks>
blockParser :: Parsec [Token] st [Token]
blockParser = (do   stmt <- stmtParser
                    sep <- separatorToken
                    return stmt) <|>
              (do   enclosedBlocks <- enclosedBlocksParser
                    return enclosedBlocks)


-- <enclosed_blocks> -> LEFT_BRACE <blocks> RIGHT_BRACE
enclosedBlocksParser :: Parsec [Token] st [Token]
enclosedBlocksParser = (do  leftBrace <- leftBraceToken
                            blocks <- blocksParser
                            rightBrace <- rightBraceToken
                            return leftBrace:blocks ++ [rightBrace])

stmtParser :: Parsec [Token] st [Token]
stmtParser = (do  x <- continueToken <|> breakToken
                  return [x]) <|>
             (do  x <- returnParser <|> voidCommandParser <|> compoundStmtParser <|> declrsParser <|> stmtIdParser
                  return x) <|>
             (do  derefPointer <- derefPointerParser
                  assignmentsOp <- assignmentsOpParser
                  return derefPointer ++ assignmentsOp)
-- <stmt> -> CONTINUE | BREAK | <return> | <void_command> | <compound_stmt> | <declrs> | <deref_pointer> <assignments_op> | <stmt_id>


-- <stmt_id> -> ID (<assignments_op> | <funcall_op>)
stmtIdParser :: Parsec [Token] st [Token]
stmtIdParser = (do  idd <- idToken
                    assignmentOrFuncall <- assignmentsOpParser <|> funcallOp
                    return idd:assignmentOrFuncall)


-- <struct> -> STRUCT ID LEFT_BRACE { <declrs> }+ RIGHT_BRACE
structParser :: Parsec [Token] st [Token]
structParser = (do  struct <- structToken
                    idd <- idToken
                    leftBrace <- leftBraceToken
                    declrs <- many1 (do x <- declrsParser
                                        return x)
                    rightBrace <- rightBraceToken
                    struct:idd:leftBrance:concat(declrs) ++ [rightBrace])

-- <compound_stmt> -> <control_structures> | <subprograms> | <struct>
compoundStmtParser :: Parsec [Token] st [Token]
compoundStmtParser = (do  x <- controlStructureParser <|> subprogramsParser <|> structParser
                          return (x))


-- <control_structures> -> <if> | <while> | <for>
controlStructureParser :: Parsec [Token] st [Token]
controlStructureParser =  (do   x <- whileParser <|> forParser <|> ifParser
                                return (x))


-- <subprograms> -> <func> | <proc>
subprogramsParser :: Parsec [Token] st [Token]
subprogramsParser = (do  x <- funcParser <|> procParser
                        return x)

-- <func> -> FUNC TYPE ID <enclosed_args> <enclosed_blocks>
funcParser :: Parsec [Token] st [Token]
funcParser =  (do   func <- funcToken
                    typee <- typeToken
                    idd <- idToken
                    enclosedArgs <- enclosedArgsParser
                    enclosedBlocks <- enclosedBlocksParser
                    return (func:typee:idd:enclosedArgs ++ enclosedBlocks))


-- <proc> -> PROC ID <enclosed_args> <enclosed_blocks>
procParser :: Parsec [Token] st [Token]
procParser =  (do   procc <- procToken
                    idd <- idToken
                    enclosedArgs <- enclosedArgsParser
                    enclosedBlocks <- enclosedBlocksParser
                    return (procc:idd:enclosedArgs ++ enclosedBlocks))


-- <enclosed_args> -> LEFT_PAREN <args> RIGHT_PAREN
enclosedArgs :: Parser [Token] st [Token]
enclosedArgs = (do  leftParen <- leftParenToken
                    args <- argsParser
                    rightParen <- rightParenToken
                    return leftParen:args ++ rightParen)

-- <args> -> TYPE ID { COMMA TYPE ID } | LAMBDA
argsParser :: Parsec [Token] st [Token]
argsParser = (do  typee <- typeToken
                  idd <- idToken
                  remainingArgs <- many (do   comma <- commaToken
                                              typee <- typeToken
                                              idd <- idToken
                                              return comma:typee:[idd])
                  return (typee:idd:(concat remainingArgs))) <|>
                (return [])


-- <while> -> WHILE <enclosed_expr> <enclosed_blocks>
whileParser :: Parsec [Token] st [Token]
whileParser =   (do   while <- whileToken
                      enclosedExpr <- enclosedExprParser
                      enclosedBlock <- enclosedBlockParser
                      return (while:enclosedExpr ++ enclosedBlock))


-- <for> -> FOR LEFT_PAREN [ <var_binding> ] SEMICOLON [ <expr> ] SEMICOLON [ <var_binding> ] RIGHT_PAREN <enclosed_blocks>
forParser :: Parsec [Token] st [Token]
forParser =   (do   for <- forToken
                    leftParen <- leftParenToken
                    maybeVarBinding1 <- varBindingParser <|> (return [])
                    semicolon <- separatorToken
                    maybeExpr <- exprParser <|> (return [])
                    semicolon <- separatorToken
                    maybeVarBinding2 <- varBindingParser <|> (return [])
                    rightParen <- rightParenToken
                    enclosedBlock <- enclosedBlockParser
                    return (for:leftParen:maybeVarBinding1 ++ semicolon:maybeExpr ++ semicolon:maybeVarBinding2 ++ rightParen:enclosedBlock))


-- <if> -> IF <enclosed_expr> THEN <enclosed_blocks> [ ELSE ( <if> | <enclosed_blocks> ) ]
ifParser :: Parsec [Token] st [Token]
ifParser =  (do   iff <- ifToken
                  enclosedExpr <- enclosedExprParser
                  thenn <- thenToken
                  enclosedBlock <- enclosedBlockParser
                  maybeElse <- (do  elsee <- elseToken
                                    ifOrEnclosed <- ifParser <|> enclosedBlockParser
                                    return elsee:ifOrEnclosed) <|>
                                (return [])
                  return (iff:enclosedExpr ++ thenn:enclosedBlock ++ maybeElse))


-- <return> -> RETURN [ <expr> ]
returnParser :: Parsec [Token] st [Token]
returnParser = (do  ret <- returnToken
                    maybeExpr <- exprParser <|> (return [])
                    return (ret:maybeExpr))
