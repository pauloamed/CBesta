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

import MemTable
import SubProgTable
import TypesTable
import OurState


-- <assign_expr> -> ASSIGN <expr>
assignExprParser :: ParsecT [Token] OurState IO(Type, [Token])
assignExprParser = (do  assign <- assignToken
                        (val, expr) <- exprParser
                        return (val, (assign:expr)))
