module TypesTable where

import OurState
import OurType

typesTable :: Operation -> Type -> OurState -> OurState
typesTable INSERT t (v, f, p, tl, sp, e) = (v, f, p, insertTypesTable t tl, sp, e)
typesTable _ _ _ = undefined


insertTypesTable :: Type -> [Type] -> [Type]
insertTypesTable t []  = [t]
insertTypesTable t typesTable = typesTable ++ [t]
