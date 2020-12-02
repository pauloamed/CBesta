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
addToScope x (v, subp, tl, sp, e) = (v, subp, tl, x ++ "/" ++ sp, e)

removeFromScope :: OurState -> OurState
removeFromScope ((v, heapCounter), subp, tl, sp, e) = ((removeScopeFromMemTable sp v, heapCounter), subp, tl, removeScopeHead sp, e)

getScope :: OurState -> String
getScope (_, _, _, sp, _) = sp


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
getValFromState x ((l, counter), _, _, _, _) = (getTypeFromMaxScope (filterMatchedVars x l))


getTypeFromMaxScope :: [Var] -> Type
getTypeFromMaxScope [] = NULL
getTypeFromMaxScope [(idA, spA, headA : tailA)] = headA
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



getAddrFromIdFromState :: String -> OurState -> Type -- (pointerType)
getAddrFromIdFromState idd ((v, _), _, _, _, _) = getAddrFromIdFromMemTable idd v


getAddrFromIdFromMemTable :: String -> [Var] -> Type -- (pointerType)
getAddrFromIdFromMemTable idA ((idB, spB, headB : tailB):tailVars) =
                  if (idA == idB) then (PointerType (headB, (idB, spB)))
                  else getAddrFromIdFromMemTable idA tailVars


--------------------------------------------------------------------------------
----------------------------------  SETTER   -----------------------------------
--------------------------------------------------------------------------------


memTable :: Operation -> VarParam -> OurState -> OurState
memTable UPDATE x ((l, counter), subp, t, sp, e) = ((updateMemTable x l, counter), subp, t, sp, e)
memTable INSERT (_, "heap", varVal) ((l, counter), subp, t, sp, e) = ((insertMemTable ((show counter), "heap", varVal) l, (counter + 1)), subp, t, sp, e)
memTable INSERT x ((l, counter), subp, t, sp, e) = ((insertMemTable x l, counter), subp, t, sp, e)
memTable REMOVE x ((l, counter), subp, t, sp, e) = ((removeMemTable x l, counter), subp, t, sp, e)


getAlloc :: OurState -> Type -> Type
getAlloc s t = (PointerType (t, (getCounterStr s, "heap")))


getCounterStr :: OurState -> String
getCounterStr ((l, counter), subp, t, sp, e) = (show counter)


insertMemTable :: VarParam -> [Var] -> [Var]
insertMemTable (idA, spA, typeA) []  = [(idA, spA, [typeA])]
insertMemTable (idA, spA, typeA) ((idB, spB, valListB) : l) =
                    if (idA == idB) && (spA == spB) then (idA, spA, typeA : valListB) : l
                    else (idB, spB, valListB) : insertMemTable (idA, spA, typeA) l


updateMemTable :: VarParam -> [Var] -> [Var]
updateMemTable _ [] = fail "Variable not found"
updateMemTable (idA, spA, typeA) ((idB, spB, currTypeB : valListB) : l) =
                    if (idA == idB) && (spA == spB) then (idA, spA, typeA : valListB) : l
                    else (idB, spB, currTypeB : valListB) : updateMemTable (idA, spA, typeA) l


removeMemTable :: VarParam -> [Var] -> [Var]
removeMemTable _ [] = fail "Variable not found"
removeMemTable (idA, spA, typeA) ((idB, spB, x : valListB) : l) =
                    if (idA == idB) && (spA == spB) then do
                      if (valListB == []) then l
                      else do (idB, spB, valListB) : l
                    else (idB, spB, x : valListB) : removeMemTable (idA, spA, typeA) l


removeScopeFromMemTable :: String -> [Var] -> [Var]
removeScopeFromMemTable _ [] = []
removeScopeFromMemTable spA ((idB, spB, x : valListB) : l) =
                    if (spA == spB) then do
                        if (valListB == []) then removeScopeFromMemTable spA l
                        else do (idB, spB, valListB) : removeScopeFromMemTable spA l
                    else (idB, spB, x : valListB) : removeScopeFromMemTable spA l
