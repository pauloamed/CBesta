module BasicExecUtils where

import OurState
import OurType

import Lexer
import Text.Parsec

import MemTable


-- fa: formal args, aa: actual args
declareArgs :: String -> [(String, Type)] -> [Type] -> OurState -> OurState
declareArgs sp (faHead:faTail) [] s = undefined
declareArgs sp [] (aaHead:aaTail) s = undefined
declareArgs sp [] [] s = s
declareArgs sp ((idFaHead, typeFaHead):faTail) (aaHead:aaTail) s =
    if (typeFaHead == aaHead) then do
      memTable INSERT (idFaHead, sp, aaHead) (declareArgs sp faTail aaTail s)
    else undefined



getIntFromType :: Type -> Int
getIntFromType (IntType x) = x
getIntFromType _ = undefined



getStringFromId :: Token -> String
getStringFromId (Id _ x) = x
getStringFromId (TypeId _ x) = x
getStringFromId _ = undefined


getValFromValAndModifiers :: Type -> [AccessModifier] -> Type
getValFromValAndModifiers t [] = t
getValFromValAndModifiers t (aHead:aTail) =
      getValFromValAndModifiers (getValFromValAndModifier t aHead) aTail


getValFromValAndModifier :: Type -> AccessModifier -> Type
getValFromValAndModifier (StructType (_, fields)) (StructAM field) = getStructField field fields
getValFromValAndModifier (ArrayType (_, values)) (ArrayAM index) = getArrayValFromIndex index values


getArrayValFromIndex :: Int -> [Type] -> Type
getArrayValFromIndex 0 (typesHead:typesTail) = typesHead
getArrayValFromIndex _ [] = undefined
getArrayValFromIndex x (typesHead:typesTail) = getArrayValFromIndex (x-1) typesTail


getStructField :: String -> [(String, Type)] -> Type
getStructField field [] = undefined
getStructField field ((f, fType):fields) =
        if (field == f) then do fType
        else do getStructField field fields


getBoolValue :: Type -> Bool
getBoolValue (BoolType x) = x
getBoolValue _ = undefined


getStringValue :: Type -> String
getStringValue (StringType s) = s
getStringValue _ = undefined


getAddrFromPointer :: Type -> VarParam
getAddrFromPointer (PointerType (_, (idd, sp))) = (idd, sp, NULL)
getAddrFromPointer _ = undefined


convertStringToType :: String -> Type -> Type
convertStringToType x (IntType _) = (IntType (read x))
convertStringToType x (DoubleType _) = (DoubleType (read x))
convertStringToType x (BoolType _) =
      if (x == "true") then (BoolType True)
      else do
        if (x == "false") then (BoolType False)
        else undefined
convertStringToType x (StringType _) = (StringType x)
convertStringToType _ _  = undefined


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
