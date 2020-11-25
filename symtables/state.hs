module OurState where

import Lexer
import Text.Parsec


data Type =
    IntType Int |
    DoubleType Double |
    BoolType Bool |
    StringType String |
    StructType (String, [(String, Type)]) | -- nome seguido de uma lista de nomes e Types (tipo + valor) |
    PointerType (Type, (String, String)) |
    ArrayType (Int, [Type]) | -- tamanho, lista com variaveis/valores, escopo e nome. como garantir consistencia de tipo?
    TupleType (Int, [Type]) -- lista de vars, escopo e nome
    deriving (Show, Eq)

type Var = (String, String, [Type])
type VarParam = (String, String, Type)
type Func = (String, Type, [Type])
type Proc = (String, [Type])

-- Memoria, Funcoes, Procedimentos, Tipos e EM_EXEC
type OurState = ([Var], [Func], [Proc], [Type])

{-
EM_EXEC: indica se o parser esta executando o codigo
-}

data Operation =
  INSERT |
  REMOVE |
  UPDATE
  deriving (Enum)


--
-- getStruct :: Token -> [[Token]] -> Type
-- getStruct idd [] =
-- getStruct idd declrs = StructType
--
-- getStructDeclrs :: [[Token]] -> [(String, Type)]
-- getStruct [] = []
-- getStruct () : l = (tb ta) : getStruct l
