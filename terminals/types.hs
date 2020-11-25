module TypesPrimTokens where


import Lexer
import Text.Parsec
import TerminalUtils


intToken :: ParsecT [Token] st IO (Token)
intToken = tokenPrim show update_pos get_token where
  get_token (Int p)  = Just (Int p)
  get_token _  = Nothing


doubleToken :: ParsecT [Token] st IO (Token)
doubleToken = tokenPrim show update_pos get_token where
  get_token (Double p)  = Just (Double p)
  get_token _  = Nothing


boolToken :: ParsecT [Token] st IO (Token)
boolToken = tokenPrim show update_pos get_token where
  get_token (Bool p)  = Just (Bool p)
  get_token _  = Nothing


stringToken :: ParsecT [Token] st IO (Token)
stringToken = tokenPrim show update_pos get_token where
  get_token (String p)  = Just (String p)
  get_token _  = Nothing


pointerToken :: ParsecT [Token] st IO (Token)
pointerToken = tokenPrim show update_pos get_token where
  get_token (Pointer p)  = Just (Pointer p)
  get_token _  = Nothing


listToken :: ParsecT [Token] st IO (Token)
listToken = tokenPrim show update_pos get_token where
  get_token (List p)  = Just (List p)
  get_token _  = Nothing


arrayToken :: ParsecT [Token] st IO (Token)
arrayToken = tokenPrim show update_pos get_token where
  get_token (Array p)  = Just (Array p)
  get_token _  = Nothing


hashmapToken :: ParsecT [Token] st IO (Token)
hashmapToken = tokenPrim show update_pos get_token where
  get_token (Hashmap p)  = Just (Hashmap p)
  get_token _  = Nothing


tupleToken :: ParsecT [Token] st IO (Token)
tupleToken = tokenPrim show update_pos get_token where
  get_token (Tuple p)  = Just (Tuple p)
  get_token _  = Nothing
