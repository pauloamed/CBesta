module CompoundGrammar where

import Lexer
import Text.Parsec

import FlowPrimTokens
import LiteralsPrimTokens
import MainPrimTokens
import OperatorsPrimTokens
import ScopesPrimTokens


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
                    maybeVarBinding1 <- (do   varBinding <- varBindingParser
                                              return varBinding) <|> (return [])
                    semicolon <- separatorToken
                    maybeExpr <- (do  expr <- exprParser
                                      return expr) <|> (return [])
                    semicolon <- separatorToken
                    maybeVarBinding2 <- (do   varBinding <- varBindingParser
                                              return varBinding) <|> (return [])
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
                                    ifOrEnclosed <- (do   iff <- if
                                                          return iff) <|>
                                                    (do   enclosedBlock <- enclosedBlockParser
                                                          return enclosedBlock)
                                    return elsee:ifOrEnclosed) <|>
                                (return [])
                  return (iff:enclosedExpr ++ thenn:enclosedBlock ++ maybeElse))


-- <return> -> RETURN [ <expr> ]
returnParser :: Parsec [Token] st [Token]
returnParser = (do  ret <- returnToken
                    maybeExpr <- (do  expr <- exprParser
                                      return expr) <|>
                                 (return [])
                    return (ret:maybeExpr))
