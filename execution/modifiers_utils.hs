module ModifiersReadExecUtils where

import OurState
import OurType

import Lexer
import Text.Parsec


import MemTable

-- a->b
-- a eh um ponteiro para struct
-- e nessa struct tem um campo b
-- esse campo b pode ser de qlqr tipo


getValFromValAndModifiers :: Type -> [AccessModifier] -> OurState -> Type
getValFromValAndModifiers t [] s = t
getValFromValAndModifiers t (aHead:aTail) s =
      getValFromValAndModifiers (getValFromValAndModifier t aHead s) aTail s


getValFromValAndModifier :: Type -> AccessModifier -> OurState -> Type
getValFromValAndModifier (StructType (_, fields)) (StructAM field) _ = getStructField field fields
getValFromValAndModifier (ArrayType (_, values)) (ArrayAM index) _ = getArrayValFromIndex index values
getValFromValAndModifier (PointerType (_, addr)) (P2SAM field) s = getFieldFromAddr field addr s
getValFromValAndModifier _ _ _ = undefined


getFieldFromAddr :: String -> (String, String) -> OurState -> Type
getFieldFromAddr field (idd, sp) s = 
      getStructField field (getFieldsFromStruct (getPointerValueFromState (PointerType (NULL, (idd,sp))) 1 s))
getFieldFromAddr _ _ _ = undefined


getFieldsFromStruct :: Type -> [(String, Type)]
getFieldsFromStruct (StructType (_, fields)) = fields
getFieldsFromStruct _ = undefined


getArrayValFromIndex :: Int -> [Type] -> Type
getArrayValFromIndex 0 (typesHead:typesTail) = typesHead
getArrayValFromIndex _ [] = undefined
getArrayValFromIndex x (typesHead:typesTail) = getArrayValFromIndex (x-1) typesTail


getStructField :: String -> [(String, Type)] -> Type
getStructField field [] = undefined
getStructField field ((f, fType):fields) =
        if (field == f) then do fType
        else do getStructField field fields