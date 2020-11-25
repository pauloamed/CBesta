module TypesTable where

import OurState

typesTable :: Operation -> Type -> OurState -> OurState
typesTable INSERT t (v, f, p, tl) = (v, f, p, insertTypesTable t tl)
typesTable _ _ _ = undefined


insertTypesTable :: Type -> [Type] -> [Type]
insertTypesTable t []  = [t]
insertTypesTable t typesTable = typesTable ++ [t]
