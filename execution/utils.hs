module BasicExecUtils where

import OurState

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


getSemanticType :: Token -> Type
getSemanticType (Int _) = (IntType 0)
getSemanticType (Double _) = (DoubleType 0.0)
getSemanticType (Bool _) = (BoolType False)
getSemanticType (String _) = (StringType "")
getSemanticType _ = undefined
