module Scope where

import OurState
import OurType

import Text.Parsec

import Lexer

import Control.Monad.IO.Class


--------------------------------------------------------------------------------
----------------------------------  SCOPE   ------------------------------------
--------------------------------------------------------------------------------

-- adiciona um novo escopo a pilha de escopos ativos
addToScopeList :: OurState -> OurState
addToScopeList (v, subp, tl, tailSp, True, contSubpr, loopStack) = (v, subp, tl, ((rootScope):tailSp), True, contSubpr, loopStack)
addToScopeList _ = undefined


-- remove um escopo da pilha de escopos ativos
removeFromScopeList :: OurState -> OurState
removeFromScopeList (v, subp, tl, (sp:tailSp), True, contSubpr, loopStack) = (v, subp, tl, tailSp, True, contSubpr, loopStack)
removeFromScopeList _ = undefined


-- adiciona uma string (novo escopo) ao escopo no topo da pilha de escopos ativos
addToCurrentScope :: String -> OurState -> OurState
addToCurrentScope x (v, subp, tl, (sp:tailSp), True, contSubpr, loopStack) = 
        (v, subp, tl, ((x ++ "/" ++ sp):tailSp), True, contSubpr, loopStack)
addToCurrentScope _ _ = undefined


-- remove a ultima string (ultimo escopo) do escopo no topo da pilha de escopos ativos
removeFromCurrentScope :: OurState -> OurState
removeFromCurrentScope ((v, heapCounter), subp, tl, (sp:tailSp), True, contSubpr, loopStack) =
        ((removeScopeFromMemTable sp v, heapCounter), subp, tl, ((removeScopeHead sp):tailSp), True, contSubpr, loopStack)
removeFromCurrentScope _ = undefined


-- remove todos os valores nos topos das variaveis que estao nesse escopo velho que foi removido
removeScopeFromMemTable :: String -> [Var] -> [Var]
removeScopeFromMemTable _ [] = []
removeScopeFromMemTable spA ((idB, spB, x : valListB) : l) =
                    if (spA == spB) then do
                        if (valListB == []) then removeScopeFromMemTable spA l
                        else do (idB, spB, valListB) : removeScopeFromMemTable spA l
                    else (idB, spB, x : valListB) : removeScopeFromMemTable spA l


-- getter do escopo atual
getScope :: OurState -> String
getScope (_, _, _, (sp:tailSp), _, _, _) = sp


-- aux pra remocao do escopo no topo de string de escopo
removeScopeHead :: String -> String
removeScopeHead "$" = "$"
removeScopeHead ('/' : x) = x
removeScopeHead (a : x) = removeScopeHead x


-- funcao pra saber se A eh sufixo de B. A eh a lista e B eh o escopo atual
isSuffixOf :: String -> String -> Bool
isSuffixOf _ "" = False
isSuffixOf s (hx:tx) =
        if s == (hx:tx) then True
        else isSuffixOf s tx



