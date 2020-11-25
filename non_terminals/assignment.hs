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


-- <assignments> -> <assignment> <remaining_assign>
assignmentsParser :: ParsecT [Token] OurState IO([Token])
assignmentsParser = (do   assignment <- assignmentParser
                          remaining <- remainingAssignsParser
                          return (assignment ++ remaining))


-- <assignments_op> -> <assign_expr> <remaining_assign>
assignmentsOpParser :: ParsecT [Token] OurState IO([Token])
assignmentsOpParser = (do   assignExpr <- assignExprParser
                            remaining <- remainingAssignsParser
                            return (assignExpr ++ remaining))


{-
ID = <expr> OU *ID = <expr>

Isso ficando latente, retornaria uma tupla
  - (ID, Escopo, (Type + Val)) [quando atribuo a ID]
  - ??? [quando atribuo a *ID ->
      posso criar um parametro extra na tupla, dizendo que eh referencia indireta
      NAO posso copiar o valor em *ID pq ainda nao ta setado]
que so seria executada quando o bloco fosse executado (quando?)

-}
-- <assignment> -> (<id> | <deref_pointer>) <assign_expr>
assignmentParser :: ParsecT [Token] OurState IO([Token])
assignmentParser = (do  x <- idParser <|> derefPointerParser
                        assignExpr <- assignExprParser
                        return (x ++ assignExpr))


{-
Uma lista de assignments. Retornuma uma lista de tuplas especiais

MUDAR ISSO PRA SER RECURSIVO?
-}
-- <remaining_assign> -> { COMMA <assignment> }
remainingAssignsParser :: ParsecT [Token] OurState IO([Token])
remainingAssignsParser = (do  remaining <- many (do   comma <- commaToken
                                                      assignment <- assignmentParser
                                                      return (comma:assignment))
                              return (concat(remaining)))
