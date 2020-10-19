module LiteralsPrimTokens where


import Lexer
import Text.Parsec
import TerminalUtils


doubleToken :: Parsec [Token] st Token
doubleToken = tokenPrim show update_pos get_token where
  get_token (Double x) = Just (Double x)
  get_token _  = Nothing


intToken :: Parsec [Token] st Token
intToken = tokenPrim show update_pos get_token where
  get_token (Int x) = Just (Int x)
  get_token _  = Nothing


boolToken :: Parsec [Token] st Token
boolToken = tokenPrim show update_pos get_token where
  get_token (Bool x) = Just (Bool x)
  get_token _  = Nothing


stringToken :: Parsec [Token] st Token
stringToken = tokenPrim show update_pos get_token where
  get_token (String x) = Just (String x)
  get_token _  = Nothing
