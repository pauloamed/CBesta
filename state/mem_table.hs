module MemTable where

import OurState
import OurType

import Text.Parsec

import Lexer

import Control.Monad.IO.Class


--------------------------------------------------------------------------------
----------------------------------  SCOPE   ------------------------------------
--------------------------------------------------------------------------------


addToScopeList :: OurState -> OurState
addToScopeList (v, subp, tl, tailSp, e, contSubpr, loopStack) = (v, subp, tl, ((rootScope):tailSp), e, contSubpr, loopStack)


removeFromScopeList :: OurState -> OurState
removeFromScopeList (v, subp, tl, (sp:tailSp), e, contSubpr, loopStack) = (v, subp, tl, tailSp, e, contSubpr, loopStack)


addToCurrentScope :: String -> OurState -> OurState
addToCurrentScope x (v, subp, tl, (sp:tailSp), e, contSubpr, loopStack) = (v, subp, tl, ((x ++ "/" ++ sp):tailSp), e, contSubpr, loopStack)


removeFromCurrentScope :: OurState -> OurState
removeFromCurrentScope ((v, heapCounter), subp, tl, (sp:tailSp), e, contSubpr, loopStack) = ((removeScopeFromMemTable sp v, heapCounter), subp, tl, ((removeScopeHead sp):tailSp), e, contSubpr, loopStack)


getScope :: OurState -> String
getScope (_, _, _, (sp:tailSp), _, _, _) = sp


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
getValFromState x ((l, _), _, _, _, _, _, _) = (getTypeFromVar (getVarFromMaxScope (filterMatchedVars x l)))


getTypeFromVar :: Var -> Type
getTypeFromVar (idA, spA, (typeHeadA, counterHeadA) : tailA) = typeHeadA


getVarFromMaxScope :: [Var] -> Var
getVarFromMaxScope [] = undefined
getVarFromMaxScope [x] = x
getVarFromMaxScope ((idA, spA, headA : tailA):(idB, spB, headB : tailB):tailVars) =
              if (length spA > length spB) then do
                getVarFromMaxScope ((idA, spA, headA : tailA):tailVars)
              else do
                getVarFromMaxScope ((idB, spB, headB : tailB):tailVars)


getAddrFromIdFromState :: String -> OurState -> Type -- (pointerType)
getAddrFromIdFromState idd ((v, _), _, _, _, _, _, _) = getAddrFromIdFromMemTable idd v


-- tem que ter controle de escopo, nao ta tendo
getAddrFromIdFromMemTable :: String -> [Var] -> Type -- (pointerType)
getAddrFromIdFromMemTable idA ((idB, spB, (typeHeadB, _) : tailB):tailVars) =
                  if (idA == idB) then (PointerType (typeHeadB, (idB, spB)))
                  else getAddrFromIdFromMemTable idA tailVars
getAddrFromIdFromMemTable _ _ = undefined


getVarToUpdate :: Int -> VarParam -> [Var] -> Var
getVarToUpdate counterActive varQuery l =
  getVarFromMaxScope (filterMatchedVars varQuery ((filterOnlyActiveVars counterActive l) ++ (filterOnlyGlobals l)))

--------------------------------------------------------------------------------
----------------------------------  FILTERS   -----------------------------------
--------------------------------------------------------------------------------

filterMatchedVars :: VarParam -> [Var] -> [Var]
filterMatchedVars (idA, spA, _) [] = []
filterMatchedVars (idA, spA, tA) ((idB, spB, headB : tailB):tailVars) =
            if ((idA == idB) && (isSuffixOf spB spA)) then do
              ((idB, spB, headB : tailB): (filterMatchedVars (idA, spA, tA) tailVars))
            else (filterMatchedVars (idA, spA, tA) tailVars)
filterMatchedVars _ _ = undefined


filterOnlyGlobals :: [Var] -> [Var]
filterOnlyGlobals [] = []
filterOnlyGlobals ((idA, spA, headA : tailA):tailVars) =
        if (spA == rootScope) then do ((idA, spA, headA : tailA): (filterOnlyGlobals tailVars))
        else do (filterOnlyGlobals tailVars)


filterOnlyActiveVars :: Int -> [Var] -> [Var]
filterOnlyActiveVars _ [] = []
filterOnlyActiveVars x ((idA, spA, (typeA, counterA) : tailA):tailVars) =
        if (x == counterA) then do
          ((idA, spA, (typeA, counterA) : tailA): (filterOnlyActiveVars x tailVars))
        else do (filterOnlyActiveVars x tailVars)


--------------------------------------------------------------------------------
----------------------------------  SETTER   -----------------------------------
--------------------------------------------------------------------------------


memTable :: Operation -> VarParam -> OurState -> OurState
memTable UPDATE x ((l, counter), subp, t, sp, e, contSubpr, loopStack) = ((updateMemTable x contSubpr l, contSubpr), subp, t, sp, e, contSubpr, loopStack)
memTable INSERT (_, "$$", varVal) ((l, counter), subp, t, sp, e, contSubpr, loopStack) = ((insertMemTable 0 ((show counter), heapScope, varVal) l, (counter + 1)), subp, t, sp, e, contSubpr, loopStack)
memTable INSERT x ((l, counter), subp, t, sp, e, contSubpr, loopStack) = ((insertMemTable contSubpr x l, counter), subp, t, sp, e, contSubpr, loopStack)
memTable REMOVE x ((l, counter), subp, t, sp, e, contSubpr, loopStack) = ((removeMemTable x l, counter), subp, t, sp, e, contSubpr, loopStack)


getVarToUpdateTest :: VarParam -> OurState -> Var
getVarToUpdateTest x ((l, counter), subp, t, sp, e, contSubpr, loopStack) = (getVarToUpdate contSubpr x l)

-- 1.filtrar somente as vars globais ou da minha ativacao
-- 2.filtrar somente as vars que dividem escopo comigo
updateMemTable :: VarParam -> Int -> [Var] -> [Var]
updateMemTable x counterActive l = updateMemTableAux x (getVarToUpdate counterActive x l) l


updateMemTableAux :: VarParam -> Var -> [Var] -> [Var]
updateMemTableAux _ _ [] = undefined
updateMemTableAux (idA, spA, typeA) (idVar, spVar, x) ((idB, spB, (typeHeadB, counterHeadB) : valListB) : l) =
                    if (idVar == idB) && (spVar == spB) then (idVar, spVar, (typeA, counterHeadB) : valListB) : l
                    else (idB, spB, (typeHeadB, counterHeadB) : valListB) : updateMemTableAux (idA, spA, typeA) (idVar, spVar, x) l




getAlloc :: OurState -> Type -> Type
getAlloc s t = (PointerType (t, (getCounterStr s, heapScope)))


getCounterStr :: OurState -> String
getCounterStr ((l, counter), subp, t, sp, e, contSubpr, loopStack) = (show counter)


insertMemTable :: Int -> VarParam -> [Var] -> [Var]
insertMemTable activeSubpr (idA, spA, typeA) []  = [(idA, spA, [(typeA, activeSubpr)])]
insertMemTable activeSubpr (idA, spA, typeA) ((idB, spB, (typeHeadB, counterHeadB) : valListB) : l) =
                    if (idA == idB) && (spA == spB) then do
                      if activeSubpr > counterHeadB then do
                        (idA, spA, (typeA, activeSubpr) : (typeHeadB, counterHeadB) : valListB) : l
                      else undefined
                    else (idB, spB, (typeHeadB, counterHeadB) : valListB) : insertMemTable activeSubpr (idA, spA, typeA) l
insertMemTable activeSubpr (idA, spA, typeA) ((idB, spB, []) : l) = undefined





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
