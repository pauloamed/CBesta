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

--
-- DECLR SEMANTICAMENTE SEMPRE VAI RESPONDER UMA LISTA DE (STRING, TYPE) = (NOME, TIPO E VALOR)
--
--


-- <declrs> -> <type> <maybe_assigned_id>  { COMMA <maybe_assigned_id>}]
declrsParser :: ParsecT [Token] OurState IO([Token])
declrsParser = (do  typee <- typeParser
                    assignedId <- maybeAssignedIdParser
                    maybeAssigns <- many (do  comma <- commaToken
                                              maybeAssignedIds <- maybeAssignedIdParser
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
maybeAssignedIdParser :: ParsecT [Token] OurState IO([Token])
maybeAssignedIdParser = (do   idd <- idToken
                              maybeAssignExpr <- assignExprParser <|> (return [])
                              return (idd:maybeAssignExpr))
