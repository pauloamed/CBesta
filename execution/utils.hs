module BasicExecUtils where

import OurState
import OurType

import Lexer
import Text.Parsec

import MemTable


-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

-- fa: formal args, aa: actual args
declareArgs :: String -> [(String, Type)] -> [Type] -> OurState -> OurState
declareArgs sp (faHead:faTail) [] s = undefined
declareArgs sp [] (aaHead:aaTail) s = undefined
declareArgs sp [] [] s = s
declareArgs sp ((idFaHead, typeFaHead):faTail) (aaHead:aaTail) s =
    if ((getDefaultValue typeFaHead) == (getDefaultValue aaHead)) then do
      memTable INSERT [] (idFaHead, sp, aaHead) (declareArgs sp faTail aaTail s)
    else do
      undefined


-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------


getIntValue :: Type -> Int
getIntValue (IntType x) = x
getIntValue _ = undefined


getBoolValue :: Type -> Bool
getBoolValue (BoolType x) = x
getBoolValue _ = undefined


getStringValue :: Type -> String
getStringValue (StringType s) = s
getStringValue _ = undefined


-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------


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


getDefaultValue :: Type -> Type
getDefaultValue (IntType _) = (IntType 0)
getDefaultValue (DoubleType _) = (DoubleType 0.0)
getDefaultValue (BoolType _) = (BoolType False)
getDefaultValue (StringType _) = (StringType "")
getDefaultValue (StructType (idd, _)) = (StructType (idd, []))
getDefaultValue (PointerType (typee, _)) = (PointerType (typee, ("","")))
getDefaultValue (ArrayType (sizee, (arrayType:_))) = (ArrayType (sizee, [getDefaultValue arrayType]))
getDefaultValue NULL = NULL

-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------


getStringFromId :: Token -> String
getStringFromId (Id _ x) = x
getStringFromId (TypeId _ x) = x
getStringFromId _ = undefined


getLiteralType :: Token -> Type
getLiteralType (IntLit _ x) = (IntType x)
getLiteralType (DoubleLit _ x) = (DoubleType x)
getLiteralType (BoolLit _ x) = (BoolType x)
getLiteralType (StringLit _ x) = (StringType x)
getLiteralType _ = undefined