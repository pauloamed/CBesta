module MemTable where

import OurState
import OurType

import Text.Parsec

import Lexer

import Control.Monad.IO.Class


--------------------------------------------------------------------------------
----------------------------------  SCOPE   ------------------------------------
--------------------------------------------------------------------------------


addToScope :: String -> OurState -> OurState
addToScope x (v, subp, tl, sp, e, contSubpr) = (v, subp, tl, x ++ "/" ++ sp, e, contSubpr)

removeFromScope :: OurState -> OurState
removeFromScope ((v, heapCounter), subp, tl, sp, e, contSubpr) = ((removeScopeFromMemTable sp v, heapCounter), subp, tl, removeScopeHead sp, e, contSubpr)

getScope :: OurState -> String
getScope (_, _, _, sp, _, _) = sp


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

--------------------------------------------------------------------------------
----------------------------------  GETTER   -----------------------------------
--------------------------------------------------------------------------------


getValFromState :: VarParam -> OurState -> Type
getValFromState x ((l, _), _, _, _, _, _) = (getTypeFromMaxScope (filterMatchedVars x l))


getTypeFromMaxScope :: [Var] -> Type
getTypeFromMaxScope [] = NULL
getTypeFromMaxScope [(idA, spA, (typeHeadA, counterHeadA) : tailA)] = typeHeadA
getTypeFromMaxScope ((idA, spA, headA : tailA):(idB, spB, headB : tailB):tailVars) =
              if (length spA > length spB) then do
                getTypeFromMaxScope ((idA, spA, headA : tailA):tailVars)
              else do
                getTypeFromMaxScope ((idB, spB, headB : tailB):tailVars)


filterMatchedVars :: VarParam -> [Var] -> [Var]
filterMatchedVars (idA, spA, _) [] = []
filterMatchedVars (idA, spA, tA) ((idB, spB, headB : tailB):tailVars) =
            if ((idA == idB) && (isSuffixOf spB spA)) then do
              ((idB, spB, headB : tailB): (filterMatchedVars (idA, spA, tA) tailVars))
            else (filterMatchedVars (idA, spA, tA) tailVars)
filterMatchedVars _ _ = undefined



getAddrFromIdFromState :: String -> OurState -> Type -- (pointerType)
getAddrFromIdFromState idd ((v, _), _, _, _, _, _) = getAddrFromIdFromMemTable idd v


-- tem que ter controle de escopo, nao ta tendo
getAddrFromIdFromMemTable :: String -> [Var] -> Type -- (pointerType)
getAddrFromIdFromMemTable idA ((idB, spB, (typeHeadB, _) : tailB):tailVars) =
                  if (idA == idB) then (PointerType (typeHeadB, (idB, spB)))
                  else getAddrFromIdFromMemTable idA tailVars


--------------------------------------------------------------------------------
----------------------------------  SETTER   -----------------------------------
--------------------------------------------------------------------------------


memTable :: Operation -> VarParam -> OurState -> OurState
memTable UPDATE x ((l, counter), subp, t, sp, e, contSubpr) = ((updateMemTable x l, counter), subp, t, sp, e, contSubpr)
memTable INSERT (_, "heap", varVal) ((l, counter), subp, t, sp, e, contSubpr) = ((insertMemTable 0 ((show counter), "heap", varVal) l, (counter + 1)), subp, t, sp, e, contSubpr)
memTable INSERT x ((l, counter), subp, t, sp, e, contSubpr) = ((insertMemTable contSubpr x l, counter), subp, t, sp, e, contSubpr)
memTable REMOVE x ((l, counter), subp, t, sp, e, contSubpr) = ((removeMemTable x l, counter), subp, t, sp, e, contSubpr)


getAlloc :: OurState -> Type -> Type
getAlloc s t = (PointerType (t, (getCounterStr s, "heap")))


getCounterStr :: OurState -> String
getCounterStr ((l, counter), subp, t, sp, e, contSubpr) = (show counter)


insertMemTable :: Int -> VarParam -> [Var] -> [Var]
insertMemTable activeSubpr (idA, spA, typeA) []  = [(idA, spA, [(typeA, activeSubpr)])]
insertMemTable activeSubpr (idA, spA, typeA) ((idB, spB, (typeHeadB, counterHeadB) : valListB) : l) =
                    if (idA == idB) && (spA == spB) then do
                      if activeSubpr > counterHeadB then do
                        (idA, spA, (typeA, activeSubpr) : (typeHeadB, counterHeadB) : valListB) : l
                      else undefined
                    else (idB, spB, (typeHeadB, counterHeadB) : valListB) : insertMemTable activeSubpr (idA, spA, typeA) l
insertMemTable activeSubpr (idA, spA, typeA) ((idB, spB, []) : l) = undefined


updateMemTable :: VarParam -> [Var] -> [Var]
updateMemTable _ [] = fail "Variable not found"
updateMemTable (idA, spA, typeA) ((idB, spB, (typeHeadB, counterHeadB) : valListB) : l) =
                    if (idA == idB) && (spA == spB) then (idA, spA, (typeA, counterHeadB) : valListB) : l
                    else (idB, spB, (typeHeadB, counterHeadB) : valListB) : updateMemTable (idA, spA, typeA) l


removeMemTable :: VarParam -> [Var] -> [Var] -- typeA eh nulo
removeMemTable _ [] = fail "Variable not found"
removeMemTable (idA, spA, typeA) ((idB, spB, (x : valListB)) : l) =
                    if (idA == idB) && (spA == spB) then do
                      if (valListB == []) then l
                      else do (idB, spB, valListB) : l
                    else (idB, spB, (x : valListB)) : removeMemTable (idA, spA, typeA) l
removeMemTable (idA, spA, typeA) ((idB, spB, []) : l) = undefined


removeScopeFromMemTable :: String -> [Var] -> [Var]
removeScopeFromMemTable _ [] = []
removeScopeFromMemTable spA ((idB, spB, x : valListB) : l) =
                    if (spA == spB) then do
                        if (valListB == []) then removeScopeFromMemTable spA l
                        else do (idB, spB, valListB) : removeScopeFromMemTable spA l
                    else (idB, spB, x : valListB) : removeScopeFromMemTable spA l
