module BindingGrammar where

import Lexer
import Text.Parsec

import FlowPrimTokens
import LiteralsPrimTokens
import MainPrimTokens
import OperatorsPrimTokens
import ScopesPrimTokens
import TypesPrimTokens
import CommandsPrimTokens


import TypeGrammar
import ExprGrammar

-- <var_binding> -> <assignments> | <declrs>
varBindingParser :: Parsec [Token] st [Token]
varBindingParser = (do  x <- assignmentsParser <|> declrsParser
                        return x)


-- <declrs> -> <type> ID [ <assign_expr> { COMMA ID <assign_expr>}]
declrsParser :: Parsec [Token] st [Token]
declrsParser = (do  typee <- typeParser
                    idd <- idToken
                    maybeAssigns <- (do   assignExpr <- assignExprParser
                                          remeaning <- many (do   comma <- commaToken
                                                                  idd <- idToken
                                                                  assignExpr <- assignExprParser
                                                                  return (comma:idd:assignExpr))
                                          return (assignExpr ++ concat(remeaning))) <|> (return [])
                    return (typee ++ idd:maybeAssigns))


-- <assign_expr> -> ASSIGN <expr>
assignExprParser :: Parsec [Token] st [Token]
assignExprParser = (do  assign <- assignToken
                        expr <- exprParser
                        return (assign:expr))


-- <assignments> -> <assignment> { COMMA <assignment> }
assignmentsParser :: Parsec [Token] st [Token]
assignmentsParser = (do   assignment <- assignmentParser
                          remeaning <- many (do   comma <- commaToken
                                                  assignment <- assignmentParser
                                                  return (comma:assignment))
                          return (assignment ++ concat(remeaning)))


-- <assignments_op> -> <assign_expr> { COMMA <assignment> }
assignmentsOpParser :: Parsec [Token] st [Token]
assignmentsOpParser = (do   assignExpr <- assignExprParser
                            remeaning <- many (do   comma <- commaToken
                                                    assignment <- assignmentParser
                                                    return (comma:assignment))
                            return (assignExpr ++ concat(remeaning)))


-- <assignment> -> ID <assign_expr> | <deref_pointer> <assign_expr>
assignmentParser :: Parsec [Token] st [Token]
assignmentParser = (do  idd <- idToken
                        assignExpr <- assignExprParser
                        return (idd:assignExpr)) <|>
                   (do  derefPointer <- derefPointerParser
                        assignExpr <- assignExprParser
                        return (derefPointer ++ assignExpr))


-- <void_command> -> <free> | <print> | <read>
voidCommandParser :: Parsec [Token] st [Token]
voidCommandParser = (do   x <- freeParser <|> printParser <|> readParser
                          return x)


-- <free> -> FREE <expr>
freeParser :: Parsec [Token] st [Token]
freeParser = (do  free <- freeToken
                  expr <- exprParser
                  return (free:expr))

-- <print> -> PRINT LEFT_PAREN <expr> RIGHT_PAREN
printParser :: Parsec [Token] st [Token]
printParser = (do printt <- printToken
                  leftParen <- leftParenToken
                  expr <- exprParser
                  rightParen <- rightParenToken
                  return (printt:leftParen:expr ++ [rightParen]))


-- <read> -> READ LEFT_PAREN ID RIGHT_PAREN
readParser :: Parsec [Token] st [Token]
readParser = (do  readd <- readToken
                  leftParen <- leftParenToken
                  idd <- idToken
                  rightParen <- rightParenToken
                  return (readd:leftParen:idd:[rightParen]))
