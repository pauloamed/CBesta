module TypesTable where

import OurState
import OurType

typesTable :: Operation -> Type -> OurState -> OurState
typesTable INSERT t (v, subp, tl, sp, e) = (v, subp, insertTypesTable t tl, sp, e)
typesTable _ _ _ = undefined


getTypeFromState :: String -> OurState -> Type
getTypeFromState x (_, _, tl, _, _) = (getTypeFromTypesTable x tl)


getTypeFromTypesTable :: String -> [Type] -> Type
getTypeFromTypesTable x [] = undefined
getTypeFromTypesTable x ((StructType (name, attribs)):typesTail) =
          if x == name then do
            (StructType (name, attribs))
          else do getTypeFromTypesTable x typesTail


insertTypesTable :: Type -> [Type] -> [Type]
insertTypesTable t []  = [t]
insertTypesTable t typesTable = typesTable ++ [t]
