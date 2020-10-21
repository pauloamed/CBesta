module ScopesPrimTokens where


import Lexer
import Text.Parsec
import TerminalUtils


leftBraceToken :: Parsec [Token] st Token
leftBraceToken = tokenPrim show update_pos get_token where
  get_token (LeftBrace p)  = Just (LeftBrace p)
  get_token _  = Nothing


rightBraceToken :: Parsec [Token] st Token
rightBraceToken = tokenPrim show update_pos get_token where
  get_token (RightBrace p)  = Just (RightBrace p)
  get_token _  = Nothing


leftParenToken :: Parsec [Token] st Token
leftParenToken = tokenPrim show update_pos get_token where
  get_token (LeftParen p)  = Just (LeftParen p)
  get_token _  = Nothing


rightParenToken :: Parsec [Token] st Token
rightParenToken = tokenPrim show update_pos get_token where
  get_token (RightParen p)  = Just (RightParen p)
  get_token _  = Nothing


leftBracketToken :: Parsec [Token] st Token
leftBracketToken = tokenPrim show update_pos get_token where
  get_token (LeftBracket p)  = Just (LeftBracket p)
  get_token _  = Nothing


rightBracketToken :: Parsec [Token] st Token
rightBracketToken = tokenPrim show update_pos get_token where
  get_token (RightBracket p)  = Just (RightBracket p)
  get_token _  = Nothing
