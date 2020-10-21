module FlowPrimTokens where


import Lexer
import Text.Parsec
import TerminalUtils


ifToken :: Parsec [Token] st Token
ifToken = tokenPrim show update_pos get_token where
  get_token (If p)  = Just (If p)
  get_token _  = Nothing


thenToken :: Parsec [Token] st Token
thenToken = tokenPrim show update_pos get_token where
  get_token (Then p)  = Just (Then p)
  get_token _  = Nothing


elseToken :: Parsec [Token] st Token
elseToken = tokenPrim show update_pos get_token where
  get_token (Else p)  = Just (Else p)
  get_token _  = Nothing


whileToken :: Parsec [Token] st Token
whileToken = tokenPrim show update_pos get_token where
  get_token (While p)  = Just (While p)
  get_token _  = Nothing


forToken :: Parsec [Token] st Token
forToken = tokenPrim show update_pos get_token where
  get_token (For p)  = Just (For p)
  get_token _  = Nothing


continueToken :: Parsec [Token] st Token
continueToken = tokenPrim show update_pos get_token where
  get_token (Continue p)  = Just (Continue p)
  get_token _  = Nothing


breakToken :: Parsec [Token] st Token
breakToken = tokenPrim show update_pos get_token where
  get_token (Break p)  = Just (Break p)
  get_token _  = Nothing
