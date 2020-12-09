module TypesTable where

import OurState
import OurType

typesTable :: Operation -> Type -> OurState -> OurState
typesTable INSERT t (v, subp, tl, sp, True, contSubpr, loopStack) = (v, subp, insertTypesTable t tl, sp, True, contSubpr, loopStack)
typesTable _ _ _ = undefined


updateType :: String -> [(String, Type)] -> OurState -> OurState
updateType idd declrs (v, subp, tl, sp, True, contSubpr, loopStack) = (v, subp, updateTypesTable idd declrs tl, sp, True, contSubpr, loopStack)


updateTypesTable :: String -> [(String, Type)] -> [Type] -> [Type]
updateTypesTable idd declrs [] = undefined
updateTypesTable idd declrs ((StructType (name, attribs)):typeTail) =
  if (idd == name) then do 
    (StructType (name, (attribs ++ declrs)):typeTail)
  else do ((StructType (name, attribs)):updateTypesTable idd declrs typeTail)


getTypeFromState :: String -> OurState -> Type
getTypeFromState x (_, _, tl, _, _, _, _) = (getTypeFromTypesTable x tl)


getTypeFromTypesTable :: String -> [Type] -> Type
getTypeFromTypesTable x [] = undefined
getTypeFromTypesTable x ((StructType (name, attribs)):typesTail) =
          if x == name then do
            (StructType (name, attribs))
          else do getTypeFromTypesTable x typesTail


insertTypesTable :: Type -> [Type] -> [Type]
insertTypesTable (StructType (idA, fieldsA)) []  = [(StructType (idA, fieldsA))]
insertTypesTable (StructType (idA, fieldsA)) ((StructType (idB, fieldsB)):typesTail) = 
      if(idA == idB) then 
        undefined
      else ((StructType (idB, fieldsB)):(insertTypesTable (StructType (idA, fieldsA)) typesTail))
