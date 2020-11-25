module MemTable where

import OurState





memTable :: Operation -> VarParam -> OurState -> OurState
memTable INSERT x (l, f, p, t) = (insertMemTable x l, f, p, t)
memTable REMOVE x (l, f, p, t) = (removeMemTable x l, f, p, t)
memTable UPDATE x (l, f, p, t) = (updateMemTable x l, f, p, t)


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
