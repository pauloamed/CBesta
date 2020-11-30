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
import OurType

import BasicExecUtils

import Control.Monad.IO.Class


-- <declrs> -> <type> <maybe_assigned_id>  { COMMA <maybe_assigned_id>}]
declrsParser :: ParsecT [Token] OurState IO([(String, Type)], [Token])
declrsParser = (do  (semanType, typee) <- typeParser
                    idd <- idToken -- aqui eh tokens mesmo
                    (val, maybeAssignExpr) <- assignExprParser <|> (return (NULL, []))

                    (tailDeclr, tokens) <- remainingDeclrsParser semanType

                    -- esse parser pode ser acessado como um statement ou como
                    -- um struct. quando acessado, pode ser que a flag de execucao
                    -- esteja desligada (struct, declaracao de funcao) ou ligada
                    -- (execucao normal). quando a flag estiver desligada, nao iremos
                    -- fazer o update das tabelas de memoria.
                    -- em ambos os casos vamos retornar uma lista de (String, Type)
                    -- a fim de poder usar esses valores na construcao de um struct

                    -- pode ser que VAL == NULL: a variavel inicial nao foi inicializada

                    s <- getState
                    if isExecOn s then do -- checando se flag de execucao ta on
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


-- <declrs> -> {<declr> SEPARATOR}+
multipleDeclrsParser :: ParsecT [Token] OurState IO([(String, Type)], [Token])
multipleDeclrsParser = (do  (hDeclrs, hTokens) <- declrsParser
                            x <- separatorToken
                            (tDeclrs, tTokens) <- multipleDeclrsParser <|> (return ([], []))
                            return (hDeclrs ++ tDeclrs, hTokens ++ x:tTokens))


-- <maybe_assigned_id> -> ID [<assign_expr>]
remainingDeclrsParser :: Type -> ParsecT [Token] OurState IO([(String, Type)], [Token])
remainingDeclrsParser typee = (do   comma <- commaToken
                                    idd <- idToken -- aqui eh token mesmo
                                    (val, maybeAssignExpr) <- assignExprParser <|> (return (NULL, []))

                                    -- pode ser que VAL == NULL: a variavel nao foi inicializada

                                    (vals, tokens) <- remainingDeclrsParser typee
                                    s <- getState
                                    if val == NULL then do
                                      updateState(memTable INSERT (getStringFromId idd, getScope s, typee))
                                      return ((getStringFromId idd, typee):vals, comma:idd:maybeAssignExpr ++ tokens)
                                    else do
                                      updateState(memTable INSERT (getStringFromId idd, getScope s, val))
                                      return ((getStringFromId idd, val):vals, comma:idd:maybeAssignExpr ++ tokens)) <|>
                              (return ([], []))
