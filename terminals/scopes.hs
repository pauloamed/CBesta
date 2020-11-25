module ScopesPrimTokens where


import Lexer
import Text.Parsec
import TerminalUtils


leftBraceToken :: ParsecT [Token] st IO (Token)
leftBraceToken = tokenPrim show update_pos get_token where
  get_token (LeftBrace p)  = Just (LeftBrace p)
  get_token _  = Nothing


rightBraceToken :: ParsecT [Token] st IO (Token)
rightBraceToken = tokenPrim show update_pos get_token where
  get_token (RightBrace p)  = Just (RightBrace p)
  get_token _  = Nothing


leftParenToken :: ParsecT [Token] st IO (Token)
leftParenToken = tokenPrim show update_pos get_token where
  get_token (LeftParen p)  = Just (LeftParen p)
  get_token _  = Nothing


rightParenToken :: ParsecT [Token] st IO (Token)
rightParenToken = tokenPrim show update_pos get_token where
  get_token (RightParen p)  = Just (RightParen p)
  get_token _  = Nothing


leftBracketToken :: ParsecT [Token] st IO (Token)
leftBracketToken = tokenPrim show update_pos get_token where
  get_token (LeftBracket p)  = Just (LeftBracket p)
  get_token _  = Nothing


rightBracketToken :: ParsecT [Token] st IO (Token)
rightBracketToken = tokenPrim show update_pos get_token where
  get_token (RightBracket p)  = Just (RightBracket p)
  get_token _  = Nothing
