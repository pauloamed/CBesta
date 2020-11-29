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

import MemTable
import SubProgTable
import TypesTable
import OurState

import ExecutionUtils


-- <assignments> -> <assignment> <remaining_assign>
assignmentsParser :: ParsecT [Token] OurState IO([Token])
assignmentsParser = (do   assignment <- assignmentParser
                          remaining <- remainingAssignsParser
                          return (assignment ++ remaining))


-- <assignments_op> -> <assign_expr> <remaining_assign>
assignmentsOpParser :: [Token] -> ParsecT [Token] OurState IO([Token])
assignmentsOpParser (idd:prefix) = (do  (exprVal, assignExpr) <- assignExprParser

                                        s <- getState
                                        updateState(memTable UPDATE (getStringFromId idd, getScope s, exprVal))

                                        remaining <- remainingAssignsParser

                                        return (assignExpr ++ remaining))


-- <assignment> -> (<id> | <deref_pointer>) <assign_expr>
assignmentParser :: ParsecT [Token] OurState IO([Token])
assignmentParser = (do  (idd:x) <- idParser <|> derefPointerParser
                        (exprVal, assignExpr) <- assignExprParser

                        s <- getState
                        updateState(memTable UPDATE (getStringFromId idd, getScope s, exprVal))

                        return ((idd:x) ++ assignExpr))


-- <remaining_assign> -> { COMMA <assignment> }
remainingAssignsParser :: ParsecT [Token] OurState IO([Token])
remainingAssignsParser = (do  remaining <- many (do   comma <- commaToken
                                                      assignment <- assignmentParser
                                                      return (comma:assignment))
                              return (concat(remaining)))
