module ExprExecUtils where

import OurState

import Lexer
import Text.Parsec


fastExpo :: Int -> Int -> Int
fastExpo x 0 = 1
fastExpo x 1 = x
fastExpo x n =
        if (mod n 2) == 0 then (fastExpo x (div n 2)) ^ 2
        else ((fastExpo x (div n 2)) ^ 2) * x


eval :: (Type, [Token]) -> Token -> (Type, [Token]) -> (Type, [Token])
eval (typeX, xTokens) opToken (typeY, yTokens) = (evalType typeX opToken typeY, xTokens ++ opToken : yTokens)


evalUnopType :: Token -> Type -> Type
evalUnopType (Negation _) (BoolType x) = (BoolType (not x))
evalUnopType (Minus _) (IntType x) = (IntType (-x))
evalUnopType (Minus _) (DoubleType x) = (DoubleType (-x))


evalType :: Type -> Token -> Type -> Type
evalType (BoolType x) (And _) (BoolType y) = BoolType (x && y)
evalType (BoolType x) (Or _) (BoolType y) = BoolType (x || y)

evalType (IntType x) (Equals _) (IntType y) = BoolType (x == y)
evalType (IntType x) (Difference _) (IntType y) = BoolType (x /= y)
evalType (IntType x) (LessThan _) (IntType y) = BoolType (x < y)
evalType (IntType x) (GreaterThan _) (IntType y) = BoolType (x > y)
evalType (IntType x) (LessEquals _) (IntType y) = BoolType (x <= y)
evalType (IntType x) (GreaterEquals _) (IntType y) = BoolType (x >= y)
evalType (IntType x) (Expo _) (IntType y) =
          if y >= 0 then IntType (fastExpo x y)
          else IntType (x ^ y)
evalType (IntType x) (Mod _) (IntType y) = IntType (mod x y)
evalType (IntType x) (Div _) (IntType y) = IntType (div x y)
evalType (IntType x) (Plus _) (IntType y) = IntType (x + y)
evalType (IntType x) (Minus _) (IntType y) = IntType (x - y)
evalType (IntType x) (Star _) (IntType y) = IntType (x * y)
-- evalType (IntType x) (Expo _) (DoubleType y) = DoubleType (x ^/ y)
evalType _ _ _ = undefined


getSubstr :: Type -> Type -> Type -> Type
getSubstr (IntType l) (IntType r) (StringType str) = (StringType (drop l (take r str)))
getSubstr _ _ _ = undefined
