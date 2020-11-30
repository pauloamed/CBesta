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

import BasicExecUtils

import ExprGrammar

import MemTable
import SubProgTable
import TypesTable

import OurState
import OurType



-- <void_command> -> <free> | <print> | <read>
voidCommandParser :: ParsecT [Token] OurState IO([Token])
voidCommandParser = (do   x <- freeParser <|> printParser <|> readParser
                          return x)


-- <free> -> FREE (<id> | <deref_pointer>)
freeParser :: ParsecT [Token] OurState IO([Token])
freeParser = (do  free <- freeToken
                  (exprVal, exprTokens) <- exprParser
                  -- exprVal tem que ser do tipo PointerType(tipo, id, escopo)
                  -- se nao for da merda...
                  -- remover (id, escopo) de memtable
                  updateState(memTable REMOVE (getAddrFromPointer exprVal))
                  return (free:exprTokens))


-- <print> -> PRINT LEFT_PAREN <expr> RIGHT_PAREN
printParser :: ParsecT [Token] OurState IO([Token])
printParser = (do printt <- printToken
                  leftParen <- leftParenToken
                  (val, expr) <- exprParser
                  rightParen <- rightParenToken

                  s <- getState
                  liftIO (print s)
                  liftIO (print "")

                  if isExecOn s then liftIO (print val)
                  else pure ()

                  return (printt:leftParen:expr ++ [rightParen]))


-- <read> -> READ LEFT_PAREN <id> RIGHT_PAREN
readParser :: ParsecT [Token] OurState IO([Token])
readParser = (do  readd <- readToken
                  leftParen <- leftParenToken
                  (idd:val) <- idParser <|> derefPointerParser -- TODO
                  rightParen <- rightParenToken

                  readVal <- liftIO (getLine)
                  s <- getState

                  updateState(memTable UPDATE (getStringFromId idd, getScope s, convertStringToType readVal (getValFromState (getStringFromId idd, getScope s, NULL) s)))

                  return (readd:leftParen:val ++ [rightParen]))
