module ModifiersExecUtils where

import OurState
import OurType

import Lexer
import Text.Parsec

import MemTable


getValFromValAndModifiers :: Type -> [AccessModifier] -> Type
getValFromValAndModifiers t [] = t
getValFromValAndModifiers t (aHead:aTail) =
      getValFromValAndModifiers (getValFromValAndModifier t aHead) aTail


getValFromValAndModifier :: Type -> AccessModifier -> Type
getValFromValAndModifier (StructType (_, fields)) (StructAM field) = getStructField field fields
getValFromValAndModifier (ArrayType (_, values)) (ArrayAM index) = getArrayValFromIndex index values
getValFromValAndModifier _ _ = undefined


getArrayValFromIndex :: Int -> [Type] -> Type
getArrayValFromIndex 0 (typesHead:typesTail) = typesHead
getArrayValFromIndex _ [] = undefined
getArrayValFromIndex x (typesHead:typesTail) = getArrayValFromIndex (x-1) typesTail


getStructField :: String -> [(String, Type)] -> Type
getStructField field [] = undefined
getStructField field ((f, fType):fields) =
        if (field == f) then do fType
        else do getStructField field fields