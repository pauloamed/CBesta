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

-- <return> -> RETURN [ <expr> ]
returnParser :: Parsec [Token] st [Token]
returnParser = (do  ret <- returnToken
                    maybeExpr <- exprParser <|> (return [])
                    return (ret:maybeExpr))



-- <enclosed_args> -> LEFT_PAREN <args> RIGHT_PAREN
enclosedArgsParser :: Parsec [Token] st [Token]
enclosedArgsParser = (do  leftParen <- leftParenToken
                          args <- argsParser
                          rightParen <- rightParenToken
                          return (leftParen:args ++ [rightParen]))

-- <args> -> <type> ID { COMMA <type> ID } | LAMBDA
argsParser :: Parsec [Token] st [Token]
argsParser = (do  typee <- typeParser
                  idd <- idToken
                  remainingArgs <- many (do   comma <- commaToken
                                              typee <- typeParser
                                              idd <- idToken
                                              return (comma:typee ++ [idd]))
                  return (typee ++ idd:(concat remainingArgs))) <|>
                (return [])
