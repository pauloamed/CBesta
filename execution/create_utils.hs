module CreateExecUtils where

import OurState
import OurType

import Lexer
import Text.Parsec

import MemTable


createSimpleType :: Token -> Type
createSimpleType (Int _) = (IntType 0)
createSimpleType (Double _) = (DoubleType 0.0)
createSimpleType (Bool _) = (BoolType False)
createSimpleType (String _) = (StringType "")
createSimpleType _ = undefined


createPointer :: Type -> Type
createPointer t = (PointerType (t, ("", "")))


createArray :: Int -> Type -> Type
createArray x elemType =
      if x > 0 then do (ArrayType (x, createTypeList x elemType))
      else undefined


createTypeList :: Int -> Type -> [Type]
createTypeList 0 _ = []
createTypeList x t = t:(createTypeList (x-1) t)


createStruct :: String -> [(String, Type)] -> Type
createStruct idd fields = StructType (idd, fields)