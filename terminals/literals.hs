module LiteralsPrimTokens where


import Lexer
import Text.Parsec
import TerminalUtils


nullToken :: ParsecT [Token] st IO (Token)
nullToken = tokenPrim show update_pos get_token where
  get_token (Null p) = Just (Null p)
  get_token _  = Nothing


doubleLitToken :: ParsecT [Token] st IO (Token)
doubleLitToken = tokenPrim show update_pos get_token where
  get_token (DoubleLit p x) = Just (DoubleLit p x)
  get_token _  = Nothing


intLitToken :: ParsecT [Token] st IO (Token)
intLitToken = tokenPrim show update_pos get_token where
  get_token (IntLit p x) = Just (IntLit p x)
  get_token _  = Nothing


boolLitToken :: ParsecT [Token] st IO (Token)
boolLitToken = tokenPrim show update_pos get_token where
  get_token (BoolLit p x) = Just (BoolLit p x)
  get_token _  = Nothing


stringLitToken :: ParsecT [Token] st IO (Token)
stringLitToken = tokenPrim show update_pos get_token where
  get_token (StringLit p x) = Just (StringLit p x)
  get_token _  = Nothing
