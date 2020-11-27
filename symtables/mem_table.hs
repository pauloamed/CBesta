module MemTable where

import OurState


import Control.Monad.IO.Class


memTable :: Operation -> VarParam -> OurState -> OurState
memTable INSERT x (l, f, p, t, sp, e) = (insertMemTable x l, f, p, t, sp, e)
memTable REMOVE x (l, f, p, t, sp, e) = (removeMemTable x l, f, p, t, sp, e)
memTable UPDATE x (l, f, p, t, sp, e) = (updateMemTable x l, f, p, t, sp, e)


getVarFromState :: VarParam -> OurState -> Type
getVarFromState x (l, _, _, _, _, _) = (getVarMemTable x l)


-- esq: ta na lista
-- dir: eh oq to pesquisando
-- da true sse esq eh sufixo do da direita
shareScope :: String -> String -> Bool
shareScope "" _ = True
shareScope _ "" = False
shareScope s (hx:tx) =
        if s == (hx:tx) then True
        else shareScope s tx


getVarMemTable :: VarParam -> [Var] -> Type
getVarMemTable (idA, spA, _) [] = (NULL)
getVarMemTable (idA, spA, x) ((idB, spB, headB : tailB):tailVars) =
                  if (idA == idB) then
                    if (shareScope spB spA) then headB
                    else getVarMemTable (idA, spA, x) tailVars
                  else getVarMemTable (idA, spA, x) tailVars


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
