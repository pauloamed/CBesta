module FlowPrimTokens where


import Lexer
import Text.Parsec
import TerminalUtils


ifToken :: Parsec [Token] st Token
ifToken = tokenPrim show update_pos get_token where
  get_token If = Just If
  get_token _  = Nothing


thenToken :: Parsec [Token] st Token
thenToken = tokenPrim show update_pos get_token where
  get_token Then = Just Then
  get_token _  = Nothing


elseToken :: Parsec [Token] st Token
elseToken = tokenPrim show update_pos get_token where
  get_token Else = Just Else
  get_token _  = Nothing


whileToken :: Parsec [Token] st Token
whileToken = tokenPrim show update_pos get_token where
  get_token While = Just While
  get_token _  = Nothing


forToken :: Parsec [Token] st Token
forToken = tokenPrim show update_pos get_token where
  get_token For = Just For
  get_token _  = Nothing


continueToken :: Parsec [Token] st Token
continueToken = tokenPrim show update_pos get_token where
  get_token Continue = Just Continue
  get_token _  = Nothing


breakToken :: Parsec [Token] st Token
breakToken = tokenPrim show update_pos get_token where
  get_token Break = Just Break
  get_token _  = Nothing
