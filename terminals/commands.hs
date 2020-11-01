module CommandsPrimTokens where


import Lexer
import Text.Parsec
import TerminalUtils


freeToken :: Parsec [Token] st Token
freeToken = tokenPrim show update_pos get_token where
  get_token (Free p)  = Just (Free p)
  get_token _  = Nothing


printToken :: Parsec [Token] st Token
printToken = tokenPrim show update_pos get_token where
  get_token (Print p)  = Just (Print p)
  get_token _  = Nothing


readToken :: Parsec [Token] st Token
readToken = tokenPrim show update_pos get_token where
  get_token (Read p)  = Just (Read p)
  get_token _  = Nothing


allocToken :: Parsec [Token] st Token
allocToken = tokenPrim show update_pos get_token where
  get_token (Alloc p)  = Just (Alloc p)
  get_token _  = Nothing


addrToken :: Parsec [Token] st Token
addrToken = tokenPrim show update_pos get_token where
  get_token (Addr p)  = Just (Addr p)
  get_token _  = Nothing


lenToken :: Parsec [Token] st Token
lenToken = tokenPrim show update_pos get_token where
  get_token (Len p)  = Just (Len p)
  get_token _  = Nothing


sizeOfToken :: Parsec [Token] st Token
sizeOfToken = tokenPrim show update_pos get_token where
  get_token (SizeOf p)  = Just (SizeOf p)
  get_token _  = Nothing
