module ExecutionUtils where

import OurState

import Lexer
import Text.Parsec

eval :: (Type, [Token]) -> Token -> (Type, [Token]) -> (Type, [Token])
eval (typeX, xTokens) opToken (typeY, yTokens) = (evalType typeX opToken typeY, xTokens ++ opToken : yTokens)


evalType :: Type -> Token -> Type -> Type
-- evalType (IntType x) modToken (IntType y) = IntType (x % y)
evalType (IntType x) (Expo _) (IntType y) = IntType (x ^ y)
evalType (IntType x) (Plus _) (IntType y) = IntType (x + y)
evalType (IntType x) (Minus _) (IntType y) = IntType (x - y)
evalType (IntType x) (Star _) (IntType y) = IntType (x * y)
evalType (IntType x) (Div _) (IntType y) = IntType (x)
evalType _ _ _ = undefined


getLiteralType :: Token -> Type
getLiteralType (IntLit _ x) = (IntType x)
getLiteralType _ = undefined
