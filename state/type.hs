module OurType where

import Lexer
import Text.Parsec


data Type =
    NULL |
    IntType Int |
    DoubleType Double |
    BoolType Bool |
    StringType String |
    StructType (String, [(String, Type)]) | -- nome seguido de uma lista de nomes e Types (tipo + valor) |
    PointerType (Type, (String, String)) |
    ArrayType (Int, [Type]) -- tamanho, lista com variaveis/valores, escopo e nome. como garantir consistencia de tipo?
    deriving (Eq)
    -- deriving (Eq, Show)

instance Show Type where
    show (IntType x) = show x
    show (DoubleType x) = show x
    show (BoolType x) = show x
    show (StringType x) = show x
    show (StructType (x, (y:_))) = id (x ++ show y)
    show (NULL) = id ">>NULL<<"
    show _ = id ">>not implemented<<"
