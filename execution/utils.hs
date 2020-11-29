module BasicExecUtils where

import OurState
import OurType

import Lexer
import Text.Parsec


getStringFromId :: Token -> String
getStringFromId (Id _ x) = x
getStringFromId (TypeId _ x) = x
getStringFromId _ = undefined


getBoolValue :: Type -> Bool
getBoolValue (BoolType x) = x
getBoolValue _ = undefined


getLiteralType :: Token -> Type
getLiteralType (IntLit _ x) = (IntType x)
getLiteralType (DoubleLit _ x) = (DoubleType x)
getLiteralType (BoolLit _ x) = (BoolType x)
getLiteralType (StringLit _ x) = (StringType x)
getLiteralType _ = undefined


createSimpleType :: Token -> Type
createSimpleType (Int _) = (IntType 0)
createSimpleType (Double _) = (DoubleType 0.0)
createSimpleType (Bool _) = (BoolType False)
createSimpleType (String _) = (StringType "")
createSimpleType _ = undefined


createPointer :: Type -> Type
createPointer t = (PointerType (t, ("", "")))


createArray :: Type -> Type -> Type
createArray (IntType x) elemType =
      if x > 0 then do (ArrayType (x, createTypeList x elemType))
      else undefined
createArray _ _ = undefined


createTypeList :: Int -> Type -> [Type]
createTypeList 0 _ = []
createTypeList x t = t:(createTypeList (x-1) t)
