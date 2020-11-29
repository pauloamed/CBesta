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
declrsParser :: ParsecT [Token] OurState IO([(String, Type)], [Token])
declrsParser = (do  (semanType, typee) <- typeParser
                    idd <- idToken
                    (val, maybeAssignExpr) <- assignExprParser <|> (return (NULL, []))

                    (tailDeclr, tokens) <- remainingDeclrsParser semanType

                    s <- getState
                    if isExecOn s then do
                      if val == NULL then do
                        updateState(memTable INSERT (getStringFromId idd, getScope s, semanType))
                        return (((getStringFromId idd, semanType):tailDeclr), (typee ++ idd:maybeAssignExpr ++ tokens))
                      else do
                        updateState(memTable INSERT (getStringFromId idd, getScope s, val))
                        return (((getStringFromId idd, val):tailDeclr), (typee ++ idd:maybeAssignExpr ++ tokens))
                    else do
                      if val == NULL then do
                        return (((getStringFromId idd, semanType):tailDeclr), (typee ++ idd:maybeAssignExpr ++ tokens))
                      else do
                        return (((getStringFromId idd, val):tailDeclr), (typee ++ idd:maybeAssignExpr ++ tokens)))



multipleDeclrsParser :: ParsecT [Token] OurState IO([(String, Type)], [Token])
multipleDeclrsParser = (do  (hDeclrs, hTokens) <- declrsParser
                            x <- separatorToken
                            (tDeclrs, tTokens) <- remainingSepDeclrsParser
                            return (hDeclrs ++ tDeclrs, hTokens ++ x:tTokens))

remainingSepDeclrsParser :: ParsecT [Token] OurState IO([(String, Type)], [Token])
remainingSepDeclrsParser = (do  (hDeclrs, hTokens) <- declrsParser
                                x <- separatorToken
                                (tDeclrs, tTokens) <- remainingSepDeclrsParser
                                return (hDeclrs ++ tDeclrs, hTokens ++ x:tTokens)) <|>
                            (return ([], []))

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

remainingDeclrsParser :: Type -> ParsecT [Token] OurState IO([(String, Type)], [Token])
remainingDeclrsParser typee = (do   comma <- commaToken
                                    idd <- idToken
                                    (val, maybeAssignExpr) <- assignExprParser <|> (return (NULL, []))

                                    (vals, tokens) <- remainingDeclrsParser typee
                                    s <- getState
                                    if val == NULL then do
                                      updateState(memTable INSERT (getStringFromId idd, getScope s, typee))
                                      return ((getStringFromId idd, typee):vals, comma:idd:maybeAssignExpr ++ tokens)
                                    else do
                                      updateState(memTable INSERT (getStringFromId idd, getScope s, val))
                                      return ((getStringFromId idd, val):vals, comma:idd:maybeAssignExpr ++ tokens)) <|>
                              (return ([], []))
