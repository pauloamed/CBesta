module VoidCommandsGrammar where

import Lexer
import Text.Parsec

import FlowPrimTokens
import LiteralsPrimTokens
import MainPrimTokens
import OperatorsPrimTokens
import ScopesPrimTokens
import TypesPrimTokens
import CommandsPrimTokens


import ExprGrammar

-- <void_command> -> <free> | <print> | <read>
voidCommandParser :: Parsec [Token] st [Token]
voidCommandParser = (do   x <- freeParser <|> printParser <|> readParser
                          return x)


-- <free> -> FREE (<id> | <deref_pointer>)
freeParser :: Parsec [Token] st [Token]
freeParser = (do  free <- freeToken
                  val <- idParser <|> derefPointerParser
                  return (free:val))


-- <print> -> PRINT LEFT_PAREN <expr> RIGHT_PAREN
printParser :: Parsec [Token] st [Token]
printParser = (do printt <- printToken
                  leftParen <- leftParenToken
                  expr <- exprParser
                  rightParen <- rightParenToken
                  return (printt:leftParen:expr ++ [rightParen]))


-- <read> -> READ LEFT_PAREN <id> RIGHT_PAREN
readParser :: Parsec [Token] st [Token]
readParser = (do  readd <- readToken
                  leftParen <- leftParenToken
                  val <- idParser <|> derefPointerParser
                  rightParen <- rightParenToken
                  return (readd:leftParen:val ++ [rightParen]))
