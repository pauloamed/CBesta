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

import Control.Monad.IO.Class

-- import System.IO.Unsafe


import ExprGrammar

import MemTable
import SubProgTable
import TypesTable
import OurState



-- <void_command> -> <free> | <print> | <read>
voidCommandParser :: ParsecT [Token] OurState IO([Token])
voidCommandParser = (do   x <- freeParser <|> printParser <|> readParser
                          return x)


-- <free> -> FREE (<id> | <deref_pointer>)
freeParser :: ParsecT [Token] OurState IO([Token])
freeParser = (do  free <- freeToken
                  val <- idParser <|> derefPointerParser
                  return (free:val))


-- <print> -> PRINT LEFT_PAREN <expr> RIGHT_PAREN
printParser :: ParsecT [Token] OurState IO([Token])
printParser = (do printt <- printToken
                  leftParen <- leftParenToken
                  (val, expr) <- exprParser
                  rightParen <- rightParenToken

                  s <- getState
                  liftIO (print s)

                  if isExecOn s then liftIO (print val)
                  else pure ()

                  return (printt:leftParen:expr ++ [rightParen]))


-- <read> -> READ LEFT_PAREN <id> RIGHT_PAREN
readParser :: ParsecT [Token] OurState IO([Token])
readParser = (do  readd <- readToken
                  leftParen <- leftParenToken
                  val <- idParser <|> derefPointerParser
                  rightParen <- rightParenToken
                  return (readd:leftParen:val ++ [rightParen]))
