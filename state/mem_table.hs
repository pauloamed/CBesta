module MemTable where

import OurState
import OurType

import Control.Monad.IO.Class


--------------------------------------------------------------------------------
----------------------------------  SCOPE   ------------------------------------
--------------------------------------------------------------------------------


addToScope :: String -> OurState -> OurState
addToScope x (v, f, p, tl, sp, e) = (v, f, p, tl, x ++ "/" ++ sp, e)

removeFromScope :: OurState -> OurState
removeFromScope (v, f, p, tl, sp, e) = (v, f, p, tl, removeScopeHead sp, e)

getScope :: OurState -> String
getScope (_, _, _, _, sp, _) = sp


removeScopeHead :: String -> String
removeScopeHead "$" = "$"
removeScopeHead ('/' : x) = x
removeScopeHead (a : x) = removeScopeHead x


shareScope :: String -> String -> Bool
shareScope "" _ = True
shareScope _ "" = False
shareScope s (hx:tx) =
        if s == (hx:tx) then True
        else shareScope s tx


--------------------------------------------------------------------------------
----------------------------------  GETTER   -----------------------------------
--------------------------------------------------------------------------------


getVarFromState :: VarParam -> OurState -> Type
getVarFromState x (l, _, _, _, _, _) = (getVarFromMemTable x l)



getVarFromMemTable :: VarParam -> [Var] -> Type
getVarFromMemTable (idA, spA, _) [] = (NULL)
getVarFromMemTable (idA, spA, x) ((idB, spB, headB : tailB):tailVars) =
                  if (idA == idB) then
                    if (shareScope spB spA) then headB
                    else getVarFromMemTable (idA, spA, x) tailVars
                  else getVarFromMemTable (idA, spA, x) tailVars


--------------------------------------------------------------------------------
----------------------------------  SETTER   -----------------------------------
--------------------------------------------------------------------------------


memTable :: Operation -> VarParam -> OurState -> OurState
memTable INSERT x (l, f, p, t, sp, e) = (insertMemTable x l, f, p, t, sp, e)
memTable REMOVE x (l, f, p, t, sp, e) = (removeMemTable x l, f, p, t, sp, e)
memTable UPDATE x (l, f, p, t, sp, e) = (updateMemTable x l, f, p, t, sp, e)


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
                    if (idA == idB) && (spA == spB) then (idA, spA, valListB) : l
                    else (idB, spB, x : valListB) : removeMemTable (idA, spA, typeA) l
