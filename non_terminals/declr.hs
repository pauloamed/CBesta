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

import MemTable
import SubProgTable
import TypesTable
import OurState

import ExecutionUtils

import Control.Monad.IO.Class


-- <declrs> -> <type> <maybe_assigned_id>  { COMMA <maybe_assigned_id>}]
declrsParser :: ParsecT [Token] OurState IO([Token])
declrsParser = (do  (semanType, typee) <- typeParser

                    assignedId <- maybeAssignedIdParser semanType
                    maybeAssigns <- many (do  comma <- commaToken
                                              maybeAssignedIds <- maybeAssignedIdParser semanType
                                              return (comma:maybeAssignedIds))
                    return ((typee ++ assignedId ++ concat(maybeAssigns))))



{-
Essa regra eh ID := <expr> OU ID
Logo ela acaba retornando OU (String, XXX) OU (String, Val)
Mas como representar Val? Dependendo do tipo esse valor varia...

Solução A:
  Passar valor de tipo ainda nao determinado : [Token]?

Solução B:
  Jogar essa regra pra regra de cima, onde a gnt ja tem acesso ao tipo desejado
  E usar esse tipo ja em maos para definir o valor atribuido
    Nested subprogram tem acesso ao escopo do pai?
-}
-- <maybe_assigned_id> -> ID [<assign_expr>]
maybeAssignedIdParser :: Type -> ParsecT [Token] OurState IO([Token])
maybeAssignedIdParser typee = (do   idd <- idToken
                                    (val, maybeAssignExpr) <- assignExprParser <|> (return (NULL, []))
                                    s <- getState
                                    if val == NULL then -- salvar na tabela com typee
                                      updateState(memTable INSERT (getStringFromId idd, getScope s, typee))
                                    else -- checar tipo e salvar na tabela usando val
                                      updateState(memTable INSERT (getStringFromId idd, getScope s, val))

                                    return ((idd:maybeAssignExpr)))
