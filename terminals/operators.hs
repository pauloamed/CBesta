module OperatorsPrimTokens where


import Lexer
import Text.Parsec
import TerminalUtils


assignToken :: Parsec [Token] st Token
assignToken = tokenPrim show update_pos get_token where
  get_token Assign = Just Assign
  get_token _  = Nothing


modToken :: Parsec [Token] st Token
modToken = tokenPrim show update_pos get_token where
  get_token Mod = Just Mod
  get_token _  = Nothing


expoToken :: Parsec [Token] st Token
expoToken = tokenPrim show update_pos get_token where
  get_token Expo = Just Expo
  get_token _  = Nothing


plusToken :: Parsec [Token] st Token
plusToken = tokenPrim show update_pos get_token where
  get_token Plus = Just Plus
  get_token _  = Nothing


minusToken :: Parsec [Token] st Token
minusToken = tokenPrim show update_pos get_token where
  get_token Minus = Just Minus
  get_token _  = Nothing


timesToken :: Parsec [Token] st Token
timesToken = tokenPrim show update_pos get_token where
  get_token Times = Just Times
  get_token _  = Nothing


divToken :: Parsec [Token] st Token
divToken = tokenPrim show update_pos get_token where
  get_token Div = Just Div
  get_token _  = Nothing


lessThanToken :: Parsec [Token] st Token
lessThanToken = tokenPrim show update_pos get_token where
  get_token LessThan = Just LessThan
  get_token _  = Nothing


greaterThanToken :: Parsec [Token] st Token
greaterThanToken = tokenPrim show update_pos get_token where
  get_token GreaterThan = Just GreaterThan
  get_token _  = Nothing


lessEqualsToken :: Parsec [Token] st Token
lessEqualsToken = tokenPrim show update_pos get_token where
  get_token LessEquals = Just LessEquals
  get_token _  = Nothing


greaterEqualsToken :: Parsec [Token] st Token
greaterEqualsToken = tokenPrim show update_pos get_token where
  get_token GreaterEquals = Just GreaterEquals
  get_token _  = Nothing


equalsToken :: Parsec [Token] st Token
equalsToken = tokenPrim show update_pos get_token where
  get_token Equals = Just Equals
  get_token _  = Nothing


differenceToken :: Parsec [Token] st Token
differenceToken = tokenPrim show update_pos get_token where
  get_token Difference = Just Difference
  get_token _  = Nothing


lessThan_Token :: Parsec [Token] st Token
lessThan_Token = tokenPrim show update_pos get_token where
  get_token LessThan_ = Just LessThan_
  get_token _  = Nothing


greaterThan_Token :: Parsec [Token] st Token
greaterThan_Token = tokenPrim show update_pos get_token where
  get_token GreaterThan_ = Just GreaterThan_
  get_token _  = Nothing


lessEquals_Token :: Parsec [Token] st Token
lessEquals_Token = tokenPrim show update_pos get_token where
  get_token LessEquals_ = Just LessEquals_
  get_token _  = Nothing


greaterEquals_Token :: Parsec [Token] st Token
greaterEquals_Token = tokenPrim show update_pos get_token where
  get_token GreaterEquals_ = Just GreaterEquals_
  get_token _  = Nothing


equals_Token :: Parsec [Token] st Token
equals_Token = tokenPrim show update_pos get_token where
  get_token Equals_ = Just Equals_
  get_token _  = Nothing


difference_Token :: Parsec [Token] st Token
difference_Token = tokenPrim show update_pos get_token where
  get_token Difference_ = Just Difference_
  get_token _  = Nothing


negationToken :: Parsec [Token] st Token
negationToken = tokenPrim show update_pos get_token where
  get_token Negation = Just Negation
  get_token _  = Nothing


andToken :: Parsec [Token] st Token
andToken = tokenPrim show update_pos get_token where
  get_token And = Just And
  get_token _  = Nothing


orToken :: Parsec [Token] st Token
orToken = tokenPrim show update_pos get_token where
  get_token Or = Just Or
  get_token _  = Nothing
