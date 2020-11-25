module OperatorsPrimTokens where


import Lexer
import Text.Parsec
import TerminalUtils


assignToken :: ParsecT [Token] st IO (Token)
assignToken = tokenPrim show update_pos get_token where
  get_token (Assign p)  = Just (Assign p)
  get_token _  = Nothing


modToken :: ParsecT [Token] st IO (Token)
modToken = tokenPrim show update_pos get_token where
  get_token (Mod p) = Just (Mod p)
  get_token _  = Nothing


expoToken :: ParsecT [Token] st IO (Token)
expoToken = tokenPrim show update_pos get_token where
  get_token (Expo p)  = Just (Expo p)
  get_token _  = Nothing


plusToken :: ParsecT [Token] st IO (Token)
plusToken = tokenPrim show update_pos get_token where
  get_token (Plus p)  = Just (Plus p)
  get_token _  = Nothing


minusToken :: ParsecT [Token] st IO (Token)
minusToken = tokenPrim show update_pos get_token where
  get_token (Minus p)  = Just (Minus p)
  get_token _  = Nothing


starToken :: ParsecT [Token] st IO (Token)
starToken = tokenPrim show update_pos get_token where
  get_token (Star p)  = Just (Star p)
  get_token _  = Nothing


divToken :: ParsecT [Token] st IO (Token)
divToken = tokenPrim show update_pos get_token where
  get_token (Div p)  = Just (Div p)
  get_token _  = Nothing


lessThanToken :: ParsecT [Token] st IO (Token)
lessThanToken = tokenPrim show update_pos get_token where
  get_token (LessThan p)  = Just (LessThan p)
  get_token _  = Nothing


greaterThanToken :: ParsecT [Token] st IO (Token)
greaterThanToken = tokenPrim show update_pos get_token where
  get_token (GreaterThan p)  = Just (GreaterThan p)
  get_token _  = Nothing


lessEqualsToken :: ParsecT [Token] st IO (Token)
lessEqualsToken = tokenPrim show update_pos get_token where
  get_token (LessEquals p)  = Just (LessEquals p)
  get_token _  = Nothing


greaterEqualsToken :: ParsecT [Token] st IO (Token)
greaterEqualsToken = tokenPrim show update_pos get_token where
  get_token (GreaterEquals p)  = Just (GreaterEquals p)
  get_token _  = Nothing


equalsToken :: ParsecT [Token] st IO (Token)
equalsToken = tokenPrim show update_pos get_token where
  get_token (Equals p)  = Just (Equals p)
  get_token _  = Nothing


differenceToken :: ParsecT [Token] st IO (Token)
differenceToken = tokenPrim show update_pos get_token where
  get_token (Difference p)  = Just (Difference p)
  get_token _  = Nothing


negationToken :: ParsecT [Token] st IO (Token)
negationToken = tokenPrim show update_pos get_token where
  get_token (Negation p)  = Just (Negation p)
  get_token _  = Nothing


andToken :: ParsecT [Token] st IO (Token)
andToken = tokenPrim show update_pos get_token where
  get_token (And p)  = Just (And p)
  get_token _  = Nothing


orToken :: ParsecT [Token] st IO (Token)
orToken = tokenPrim show update_pos get_token where
  get_token (Or p)  = Just (Or p)
  get_token _  = Nothing
