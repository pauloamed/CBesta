module FlowPrimTokens where


import Lexer
import Text.Parsec
import TerminalUtils


ifToken :: ParsecT [Token] st IO (Token)
ifToken = tokenPrim show update_pos get_token where
  get_token (If p)  = Just (If p)
  get_token _  = Nothing


thenToken :: ParsecT [Token] st IO (Token)
thenToken = tokenPrim show update_pos get_token where
  get_token (Then p)  = Just (Then p)
  get_token _  = Nothing


elseToken :: ParsecT [Token] st IO (Token)
elseToken = tokenPrim show update_pos get_token where
  get_token (Else p)  = Just (Else p)
  get_token _  = Nothing


whileToken :: ParsecT [Token] st IO (Token)
whileToken = tokenPrim show update_pos get_token where
  get_token (While p)  = Just (While p)
  get_token _  = Nothing


forToken :: ParsecT [Token] st IO (Token)
forToken = tokenPrim show update_pos get_token where
  get_token (For p)  = Just (For p)
  get_token _  = Nothing


continueToken :: ParsecT [Token] st IO (Token)
continueToken = tokenPrim show update_pos get_token where
  get_token (Continue p)  = Just (Continue p)
  get_token _  = Nothing


breakToken :: ParsecT [Token] st IO (Token)
breakToken = tokenPrim show update_pos get_token where
  get_token (Break p)  = Just (Break p)
  get_token _  = Nothing
