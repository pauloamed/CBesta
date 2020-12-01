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

import BasicExecUtils


-- <return> -> RETURN [ <expr> ]
returnParser :: ParsecT [Token] OurState IO([Token])
returnParser = (do  ret <- returnToken
                    maybeExpr <- (do  (_, x) <- exprParser
                                      return (x))
                                  <|> (return [])
                    return (ret:maybeExpr))


-- <args> -> <type> ID { COMMA <type> ID } | LAMBDA
argsParser :: ParsecT [Token] OurState IO([(String, Type)], [Token])
argsParser = (do  (semanType, tokenType) <- typeParser
                  idd <- idToken -- aqui eh token mesmo
                  (remainingArgs, remainingArgsTokens) <- remainingArgsParser
                  return (((getStringFromId idd, semanType):remainingArgs, tokenType ++ idd:remainingArgsTokens))) <|>
              (return ([], []))


remainingArgsParser :: ParsecT [Token] OurState IO([(String, Type)], [Token])
remainingArgsParser = (do   comma <- commaToken
                            (semanType, tokenType) <- typeParser
                            idd <- idToken -- aqui eh token mesmo
                            (tailArgs, tailTokenArgs) <- remainingArgsParser
                            return (((getStringFromId idd, semanType):tailArgs), comma:tokenType ++ idd:tailTokenArgs)) <|>
                      (return ([], []))
