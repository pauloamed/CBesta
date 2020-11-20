module AssignmentGrammar where

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


-- <remaining_assign> -> { COMMA <assignment> }
remainingAssignsParser :: Parsec [Token] st [Token]
remainingAssignsParser = (do  remaining <- many (do   comma <- commaToken
                                                      assignment <- assignmentParser
                                                      return (comma:assignment))
                              return (concat(remaining)))


-- <assignments> -> <assignment> <remaining_assign>
assignmentsParser :: Parsec [Token] st [Token]
assignmentsParser = (do   assignment <- assignmentParser
                          remaining <- remainingAssignsParser
                          return (assignment ++ remaining))


-- <assignments_op> -> <assign_expr> <remaining_assign>
assignmentsOpParser :: Parsec [Token] st [Token]
assignmentsOpParser = (do   assignExpr <- assignExprParser
                            remaining <- remainingAssignsParser
                            return (assignExpr ++ remaining))


-- <assignment> -> (<id> | <deref_pointer>) <assign_expr>
assignmentParser :: Parsec [Token] st [Token]
assignmentParser = (do  x <- idParser <|> derefPointerParser
                        assignExpr <- assignExprParser
                        return (x ++ assignExpr))
