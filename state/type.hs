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
    show (IntType x) = id ("Int: " ++ (show x))
    show (DoubleType x) = show x
    show (BoolType x) = show x
    show (StringType x) = show x
    show (StructType (x, fields)) = id ("(Struct : " ++ x ++ " : " ++ show fields ++ ")")
    show (ArrayType (x, els)) = id ("(Array : " ++ (show x) ++ " : " ++ show els ++ ")")
    show (PointerType (x, (idd, sp))) = id ("(Pointer : " ++ (show x) ++ " : (" ++ (show idd) ++ ", " ++ (show sp) ++ "))")
    show (NULL) = id ">>NULL<<"


data AccessModifier =
    StructAM String |
    ArrayAM Int
    deriving(Eq)


data LoopControlType =
    OK |
    CONTINUE |
    BREAK |
    RETURN
    deriving(Eq)

instance Show LoopControlType where
    show OK = "OK"
    show CONTINUE = "CONTINUE"
    show BREAK = "BREAK"
    show RETURN = "RETURN"