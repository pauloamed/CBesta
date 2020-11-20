module BindingUtilsGrammar where

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


-- <assign_expr> -> ASSIGN <expr>
assignExprParser :: Parsec [Token] st [Token]
assignExprParser = (do  assign <- assignToken
                        expr <- exprParser
                        return (assign:expr))
