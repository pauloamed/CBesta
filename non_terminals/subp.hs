module SubProgGrammar where

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

import MemTable
import SubProgTable
import TypesTable
import OurState



-- <return> -> RETURN [ <expr> ]
returnParser :: ParsecT [Token] OurState IO([Token])
returnParser = (do  ret <- returnToken
                    maybeExpr <- (do  (_, x) <- exprParser
                                      return (x))
                                  <|> (return [])
                    return (ret:maybeExpr))



-- <enclosed_args> -> LEFT_PAREN <args> RIGHT_PAREN
enclosedArgsParser :: ParsecT [Token] OurState IO([Token])
enclosedArgsParser = (do  leftParen <- leftParenToken
                          args <- argsParser
                          rightParen <- rightParenToken
                          return (leftParen:args ++ [rightParen]))

-- <args> -> <type> ID { COMMA <type> ID } | LAMBDA
argsParser :: ParsecT [Token] OurState IO([Token])
argsParser = (do  (_, typee) <- typeParser
                  idd <- idToken
                  remainingArgs <- many (do   comma <- commaToken
                                              (_, typee) <- typeParser
                                              idd <- idToken
                                              return (comma:typee ++ [idd]))
                  return (typee ++ idd:(concat remainingArgs))) <|>
                (return [])
