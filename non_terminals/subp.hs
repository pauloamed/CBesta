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
import OurType



-- <return> -> RETURN [ <expr> ]
returnParser :: ParsecT [Token] OurState IO([Token])
returnParser = (do  ret <- returnToken
                    maybeExpr <- (do  (_, x) <- exprParser
                                      return (x))
                                  <|> (return [])
                    return (ret:maybeExpr))


-- <args> -> <type> ID { COMMA <type> ID } | LAMBDA
argsParser :: ParsecT [Token] OurState IO([Type], [Token])
argsParser = (do  (semanType, tokenType) <- typeParser
                  idd <- idToken -- aqui eh token mesmo
                  (remainingArgsSeman, remainingArgsTokens) <- remainingArgsParser
                  return ((semanType:remainingArgsSeman, tokenType ++ idd:remainingArgsTokens))) <|>
              (return ([], []))


remainingArgsParser :: ParsecT [Token] OurState IO([Type], [Token])
remainingArgsParser = (do   comma <- commaToken
                            (semanType, tokenType) <- typeParser
                            idd <- idToken -- aqui eh token mesmo
                            (tailSemanTypes, tailTokenTypes) <- remainingArgsParser
                            return ((semanType:tailSemanTypes), comma:tokenType ++ idd:tailTokenTypes)) <|>
                      (return ([], []))
