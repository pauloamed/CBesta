module LiteralsPrimTokens where


import Lexer
import Text.Parsec
import TerminalUtils


doubleToken :: Parsec [Token] st Token
doubleToken = tokenPrim show update_pos get_token where
  get_token (Double p x) = Just (Double p x)
  get_token _  = Nothing


intToken :: Parsec [Token] st Token
intToken = tokenPrim show update_pos get_token where
  get_token (Int p x) = Just (Int p x)
  get_token _  = Nothing


boolToken :: Parsec [Token] st Token
boolToken = tokenPrim show update_pos get_token where
  get_token (Bool p x) = Just (Bool p x)
  get_token _  = Nothing


stringToken :: Parsec [Token] st Token
stringToken = tokenPrim show update_pos get_token where
  get_token (String p x) = Just (String p x)
  get_token _  = Nothing
