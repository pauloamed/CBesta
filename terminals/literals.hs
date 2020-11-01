module LiteralsPrimTokens where


import Lexer
import Text.Parsec
import TerminalUtils


doubleLitToken :: Parsec [Token] st Token
doubleLitToken = tokenPrim show update_pos get_token where
  get_token (DoubleLit p x) = Just (DoubleLit p x)
  get_token _  = Nothing


intLitToken :: Parsec [Token] st Token
intLitToken = tokenPrim show update_pos get_token where
  get_token (IntLit p x) = Just (IntLit p x)
  get_token _  = Nothing


boolLitToken :: Parsec [Token] st Token
boolLitToken = tokenPrim show update_pos get_token where
  get_token (BoolLit p x) = Just (BoolLit p x)
  get_token _  = Nothing


stringLitToken :: Parsec [Token] st Token
stringLitToken = tokenPrim show update_pos get_token where
  get_token (StringLit p x) = Just (StringLit p x)
  get_token _  = Nothing
