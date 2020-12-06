module ExprExecUtils where

import OurState
import OurType

import Lexer
import Text.Parsec

---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------

getSubstr :: Type -> Type -> Type -> Type
getSubstr (IntType l) (IntType r) (StringType str) = (StringType (drop l (take r str)))
getSubstr _ _ _ = undefined


getLen :: Type -> Type
getLen (StringType x) = (IntType (length x))
getLen (ArrayType (size, _)) = (IntType size)
getLen _ = undefined

---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------

fastExpo :: Int -> Int -> Int
fastExpo x 0 = 1
fastExpo x 1 = x
fastExpo x n =
        if (mod n 2) == 0 then (fastExpo x (div n 2)) ^ 2
        else ((fastExpo x (div n 2)) ^ 2) * x


---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------

eval :: (Type, [Token]) -> Token -> (Type, [Token]) -> Bool -> (Type, [Token])
eval (typeX, xTokens) opToken (typeY, yTokens) True = (evalType typeX opToken typeY, xTokens ++ opToken : yTokens)
eval (typeX, xTokens) opToken (typeY, yTokens) False = (NULL, xTokens ++ opToken : yTokens)


evalUnopType :: Token -> Type -> Type
-- BOOL
evalUnopType (Negation _) (BoolType x) = (BoolType (not x))

-- INT
evalUnopType (Minus _) (IntType x) = (IntType (-x))

-- DOUBLE
evalUnopType (Minus _) (DoubleType x) = (DoubleType (-x))


evalType :: Type -> Token -> Type -> Type
-- BOOL
evalType (BoolType x) (And _) (BoolType y) = BoolType (x && y)
evalType (BoolType x) (Or _) (BoolType y) = BoolType (x || y)
evalType (BoolType x) (Equals _) (BoolType y) = BoolType (x == y)
evalType (BoolType x) (Difference _) (BoolType y) = BoolType (x /= y)

-- STRING
evalType (StringType x) (Plus _) (StringType y) = StringType (x++y)
evalType (StringType x) (Equals _) (StringType y) = BoolType (x == y)
evalType (StringType x) (Difference _) (StringType y) = BoolType (x /= y)

-- DOUBLE -> DOUBLE
evalType (DoubleType x) (Plus _) (DoubleType y) = DoubleType (x + y)
evalType (DoubleType x) (Minus _) (DoubleType y) = DoubleType (x - y)
evalType (DoubleType x) (Star _) (DoubleType y) = DoubleType (x * y)
evalType (DoubleType x) (Div _) (DoubleType y) = DoubleType (x / y)
evalType (DoubleType x) (Div _) (DoubleType y) = DoubleType (x / y)
evalType (DoubleType x) (Expo _) (IntType y) = DoubleType (x ^ y) -- EXPO SO PRA INT

-- DOUBLE -> BOOL
evalType (DoubleType x) (LessThan _) (DoubleType y) = BoolType (x < y)
evalType (DoubleType x) (GreaterThan _) (DoubleType y) = BoolType (x > y)
evalType (DoubleType x) (LessEquals _) (DoubleType y) = BoolType (x <= y)
evalType (DoubleType x) (GreaterEquals _) (DoubleType y) = BoolType (x >= y)
evalType (DoubleType x) (Equals _) (DoubleType y) = BoolType (x == y)
evalType (DoubleType x) (Difference _) (DoubleType y) = BoolType (x /= y)

-- INT -> INT
evalType (IntType x) (Expo _) (IntType y) =
          if y >= 0 then IntType (fastExpo x y)
          else IntType (x ^ y)
evalType (IntType x) (Mod _) (IntType y) = IntType (mod x y)
evalType (IntType x) (Div _) (IntType y) = IntType (div x y)
evalType (IntType x) (Plus _) (IntType y) = IntType (x + y)
evalType (IntType x) (Minus _) (IntType y) = IntType (x - y)
evalType (IntType x) (Star _) (IntType y) = IntType (x * y)

-- INT -> BOOL
evalType (IntType x) (Equals _) (IntType y) = BoolType (x == y)
evalType (IntType x) (Difference _) (IntType y) = BoolType (x /= y)
evalType (IntType x) (LessThan _) (IntType y) = BoolType (x < y)
evalType (IntType x) (GreaterThan _) (IntType y) = BoolType (x > y)
evalType (IntType x) (LessEquals _) (IntType y) = BoolType (x <= y)
evalType (IntType x) (GreaterEquals _) (IntType y) = BoolType (x >= y)

evalType _ _ _ = undefined


---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------

cast :: Type -> Type -> Type
cast x (StringType _) = (StringType (show x)) -- qlqr coisa pra STR
cast (IntType x) (IntType _) = (IntType x) -- padrao INT
cast (BoolType x) (BoolType _) = (BoolType x) -- padrao BOOL
cast (DoubleType x) (DoubleType _) = (DoubleType x) -- padrao DOUBLE

cast (IntType x) (DoubleType _) = (DoubleType ((fromIntegral x)*1.0)) -- INT -> DOUBLE
cast (IntType x) (BoolType _) = -- INT -> BOOL
          if (x == 0) then do (BoolType False)
          else do (BoolType True)

cast (DoubleType x) (IntType _) = (IntType (floor x)) -- DOUBLE -> INT

cast (BoolType x) (IntType _) = -- BOOL -> INT
    if x then do (IntType 1)
    else do (IntType 0)
    
cast _ _ = undefined
