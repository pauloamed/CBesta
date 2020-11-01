module TypesPrimTokens where


import Lexer
import Text.Parsec
import TerminalUtils


intToken :: Parsec [Token] st Token
intToken = tokenPrim show update_pos get_token where
  get_token (Int p)  = Just (Int p)
  get_token _  = Nothing


doubleToken :: Parsec [Token] st Token
doubleToken = tokenPrim show update_pos get_token where
  get_token (Double p)  = Just (Double p)
  get_token _  = Nothing


boolToken :: Parsec [Token] st Token
boolToken = tokenPrim show update_pos get_token where
  get_token (Bool p)  = Just (Bool p)
  get_token _  = Nothing


stringToken :: Parsec [Token] st Token
stringToken = tokenPrim show update_pos get_token where
  get_token (String p)  = Just (String p)
  get_token _  = Nothing


pointerToken :: Parsec [Token] st Token
pointerToken = tokenPrim show update_pos get_token where
  get_token (Pointer p)  = Just (Pointer p)
  get_token _  = Nothing


listToken :: Parsec [Token] st Token
listToken = tokenPrim show update_pos get_token where
  get_token (List p)  = Just (List p)
  get_token _  = Nothing


arrayToken :: Parsec [Token] st Token
arrayToken = tokenPrim show update_pos get_token where
  get_token (Array p)  = Just (Array p)
  get_token _  = Nothing


hashmapToken :: Parsec [Token] st Token
hashmapToken = tokenPrim show update_pos get_token where
  get_token (Hashmap p)  = Just (Hashmap p)
  get_token _  = Nothing


tupleToken :: Parsec [Token] st Token
tupleToken = tokenPrim show update_pos get_token where
  get_token (Tuple p)  = Just (Tuple p)
  get_token _  = Nothing
