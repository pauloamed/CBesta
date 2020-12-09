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
    show (BoolType True) = id "true"
    show (BoolType False) = id "false"
    show (StringType x) = id x
    show (StructType (x, fields)) = id ("(Struct : " ++ x ++ " : " ++ show fields ++ ")")
    show (ArrayType (x, els)) = id ("(Array : " ++ (show x) ++ " : " ++ show els ++ ")")
    show (PointerType (x, (idd, sp))) = id ("(Pointer : " ++ (show x) ++ " : (" ++ (show idd) ++ ", " ++ (show sp) ++ "))")
    show (NULL) = id ">>NULL<<"


data AccessModifier =
    StructAM String |
    ArrayAM Int |
    P2SAM String 
    deriving(Eq)

instance Show AccessModifier where
    show (StructAM field) = (show "(STRUCT_AM") ++ " " ++ (show field) ++ ")"
    show (ArrayAM index) = (show "(ARRAY_AM") ++ " " ++ (show index) ++ ")"
    show (P2SAM field) = (show "(P2S_AM") ++ " " ++ (show field) ++ ")"
-- P2SAM: pointer to strcut access modifier

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