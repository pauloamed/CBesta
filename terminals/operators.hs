module OperatorsPrimTokens where


import Lexer
import Text.Parsec
import TerminalUtils


assignToken :: Parsec [Token] st Token
assignToken = tokenPrim show update_pos get_token where
  get_token (Assign p)  = Just (Assign p)
  get_token _  = Nothing


modToken :: Parsec [Token] st Token
modToken = tokenPrim show update_pos get_token where
  get_token (Mod p) = Just (Mod p)
  get_token _  = Nothing


expoToken :: Parsec [Token] st Token
expoToken = tokenPrim show update_pos get_token where
  get_token (Expo p)  = Just (Expo p)
  get_token _  = Nothing


plusToken :: Parsec [Token] st Token
plusToken = tokenPrim show update_pos get_token where
  get_token (Plus p)  = Just (Plus p)
  get_token _  = Nothing


minusToken :: Parsec [Token] st Token
minusToken = tokenPrim show update_pos get_token where
  get_token (Minus p)  = Just (Minus p)
  get_token _  = Nothing


timesToken :: Parsec [Token] st Token
timesToken = tokenPrim show update_pos get_token where
  get_token (Times p)  = Just (Times p)
  get_token _  = Nothing


divToken :: Parsec [Token] st Token
divToken = tokenPrim show update_pos get_token where
  get_token (Div p)  = Just (Div p)
  get_token _  = Nothing


lessThanToken :: Parsec [Token] st Token
lessThanToken = tokenPrim show update_pos get_token where
  get_token (LessThan p)  = Just (LessThan p)
  get_token _  = Nothing


greaterThanToken :: Parsec [Token] st Token
greaterThanToken = tokenPrim show update_pos get_token where
  get_token (GreaterThan p)  = Just (GreaterThan p)
  get_token _  = Nothing


lessEqualsToken :: Parsec [Token] st Token
lessEqualsToken = tokenPrim show update_pos get_token where
  get_token (LessEquals p)  = Just (LessEquals p)
  get_token _  = Nothing


greaterEqualsToken :: Parsec [Token] st Token
greaterEqualsToken = tokenPrim show update_pos get_token where
  get_token (GreaterEquals p)  = Just (GreaterEquals p)
  get_token _  = Nothing


equalsToken :: Parsec [Token] st Token
equalsToken = tokenPrim show update_pos get_token where
  get_token (Equals p)  = Just (Equals p)
  get_token _  = Nothing


differenceToken :: Parsec [Token] st Token
differenceToken = tokenPrim show update_pos get_token where
  get_token (Difference p)  = Just (Difference p)
  get_token _  = Nothing


lessThan_Token :: Parsec [Token] st Token
lessThan_Token = tokenPrim show update_pos get_token where
  get_token (LessThan_ p)  = Just (LessThan_ p)
  get_token _  = Nothing


greaterThan_Token :: Parsec [Token] st Token
greaterThan_Token = tokenPrim show update_pos get_token where
  get_token (GreaterThan_ p)  = Just (GreaterThan_ p)
  get_token _  = Nothing


lessEquals_Token :: Parsec [Token] st Token
lessEquals_Token = tokenPrim show update_pos get_token where
  get_token (LessEquals_ p)  = Just (LessEquals_ p)
  get_token _  = Nothing


greaterEquals_Token :: Parsec [Token] st Token
greaterEquals_Token = tokenPrim show update_pos get_token where
  get_token (GreaterEquals_ p)  = Just (GreaterEquals_ p)
  get_token _  = Nothing


equals_Token :: Parsec [Token] st Token
equals_Token = tokenPrim show update_pos get_token where
  get_token (Equals_ p)  = Just (Equals_ p)
  get_token _  = Nothing


difference_Token :: Parsec [Token] st Token
difference_Token = tokenPrim show update_pos get_token where
  get_token (Difference_ p)  = Just (Difference_ p)
  get_token _  = Nothing


negationToken :: Parsec [Token] st Token
negationToken = tokenPrim show update_pos get_token where
  get_token (Negation p)  = Just (Negation p)
  get_token _  = Nothing


andToken :: Parsec [Token] st Token
andToken = tokenPrim show update_pos get_token where
  get_token (And p)  = Just (And p)
  get_token _  = Nothing


orToken :: Parsec [Token] st Token
orToken = tokenPrim show update_pos get_token where
  get_token (Or p)  = Just (Or p)
  get_token _  = Nothing
