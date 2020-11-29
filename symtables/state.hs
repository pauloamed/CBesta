module OurState where

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
    ArrayType (Int, [Type]) | -- tamanho, lista com variaveis/valores, escopo e nome. como garantir consistencia de tipo?
    TupleType (Int, [Type]) -- lista de vars, escopo e nome
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


type Var = (String, String, [Type])
type VarParam = (String, String, Type)
type Func = (String, Type, [Type])
type Proc = (String, [Type])

-- Memoria, Funcoes, Procedimentos, Tipos e EM_EXEC
type OurState = ([Var], [Func], [Proc], [Type], String, Bool)

addToScope :: String -> OurState -> OurState
addToScope x (v, f, p, tl, sp, e) = (v, f, p, tl, x ++ "/" ++ sp, e)

removeFromScope :: OurState -> OurState
removeFromScope (v, f, p, tl, sp, e) = (v, f, p, tl, removeScopeHead sp, e)

getScope :: OurState -> String
getScope (_, _, _, _, sp, _) = sp


removeScopeHead :: String -> String
removeScopeHead "$" = "$"
removeScopeHead ('/' : x) = x
removeScopeHead (a : x) = removeScopeHead x



turnExecOn :: OurState -> OurState
turnExecOn (v, f, p, tl, sp, _) = (v, f, p, tl, sp, True)

turnExecOff :: OurState -> OurState
turnExecOff (v, f, p, tl, sp, _) = (v, f, p, tl, sp, False)

toggleExec :: OurState -> OurState
toggleExec (v, f, p, tl, sp, x) = (v, f, p, tl, sp, not x)

isExecOn :: OurState -> Bool
isExecOn (_, _, _, _, _, b) = b


{-
EM_EXEC: indica se o parser esta executando o codigo
-}

data Operation =
  INSERT |
  REMOVE |
  UPDATE
  deriving (Enum)
