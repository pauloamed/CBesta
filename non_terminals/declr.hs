module DeclrGrammar where

import Lexer
import Text.Parsec

import FlowPrimTokens
import LiteralsPrimTokens
import MainPrimTokens
import OperatorsPrimTokens
import ScopesPrimTokens
import TypesPrimTokens
import CommandsPrimTokens


import BindingUtilsGrammar
import ExprGrammar

-- <declrs> -> <type> <maybe_assigned_id>  { COMMA <maybe_assigned_id>}]
declrsParser :: Parsec [Token] st [Token]
declrsParser = (do  typee <- typeParser
                    assignedId <- maybeAssignedIdParser
                    maybeAssigns <- many (do  comma <- commaToken
                                              maybeAssignedIds <- maybeAssignedIdParser
                                              return (comma:maybeAssignedIds))
                    return (typee ++ assignedId ++ concat(maybeAssigns)))


-- <maybe_assigned_id> -> ID [<assign_expr>]
maybeAssignedIdParser :: Parsec [Token] st [Token]
maybeAssignedIdParser = (do   idd <- idToken
                              maybeAssignExpr <- assignExprParser <|> (return [])
                              return (idd:maybeAssignExpr))
