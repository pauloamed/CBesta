module CommandsPrimTokens where


import Lexer
import Text.Parsec
import TerminalUtils


freeToken :: ParsecT [Token] st IO (Token)
freeToken = tokenPrim show update_pos get_token where
  get_token (Free p)  = Just (Free p)
  get_token _  = Nothing


printToken :: ParsecT [Token] st IO (Token)
printToken = tokenPrim show update_pos get_token where
  get_token (Print p)  = Just (Print p)
  get_token _  = Nothing


readToken :: ParsecT [Token] st IO (Token)
readToken = tokenPrim show update_pos get_token where
  get_token (Read p)  = Just (Read p)
  get_token _  = Nothing


allocToken :: ParsecT [Token] st IO (Token)
allocToken = tokenPrim show update_pos get_token where
  get_token (Alloc p)  = Just (Alloc p)
  get_token _  = Nothing


addrToken :: ParsecT [Token] st IO (Token)
addrToken = tokenPrim show update_pos get_token where
  get_token (Addr p)  = Just (Addr p)
  get_token _  = Nothing


lenToken :: ParsecT [Token] st IO (Token)
lenToken = tokenPrim show update_pos get_token where
  get_token (Len p)  = Just (Len p)
  get_token _  = Nothing


castToken :: ParsecT [Token] st IO (Token)
castToken = tokenPrim show update_pos get_token where
  get_token (Cast p)  = Just (Cast p)
  get_token _  = Nothing


substrToken :: ParsecT [Token] st IO (Token)
substrToken = tokenPrim show update_pos get_token where
  get_token (Substr p)  = Just (Substr p)
  get_token _  = Nothing
