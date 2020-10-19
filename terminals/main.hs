module MainPrimTokens where


import Lexer
import Text.Parsec
import TerminalUtils


typeToken :: Parsec [Token] st Token
typeToken = tokenPrim show update_pos get_token where
  get_token (Type x) = Just (Type x)
  get_token _  = Nothing


idToken :: Parsec [Token] st Token
idToken = tokenPrim show update_pos get_token where
  get_token (Id x) = Just (Id x)
  get_token _  = Nothing


returnToken :: Parsec [Token] st Token
returnToken = tokenPrim show update_pos get_token where
  get_token Return = Just Return
  get_token _  = Nothing


hashtagToken :: Parsec [Token] st Token
hashtagToken = tokenPrim show update_pos get_token where
  get_token Hashtag = Just Hashtag
  get_token _  = Nothing


importToken :: Parsec [Token] st Token
importToken = tokenPrim show update_pos get_token where
  get_token Import = Just Import
  get_token _  = Nothing


mainToken :: Parsec [Token] st Token
mainToken = tokenPrim show update_pos get_token where
  get_token Main = Just Main
  get_token _  = Nothing


funcToken :: Parsec [Token] st Token
funcToken = tokenPrim show update_pos get_token where
  get_token Func = Just Func
  get_token _  = Nothing


procToken :: Parsec [Token] st Token
procToken = tokenPrim show update_pos get_token where
  get_token Proc = Just Proc
  get_token _  = Nothing


semicolonToken :: Parsec [Token] st Token
semicolonToken = tokenPrim show update_pos get_token where
  get_token Semicolon = Just Semicolon
  get_token _  = Nothing


colonToken :: Parsec [Token] st Token
colonToken = tokenPrim show update_pos get_token where
  get_token Colon = Just Colon
  get_token _  = Nothing


newLineToken :: Parsec [Token] st Token
newLineToken = tokenPrim show update_pos get_token where
  get_token NewLine = Just NewLine
  get_token _  = Nothing


commaToken :: Parsec [Token] st Token
commaToken = tokenPrim show update_pos get_token where
  get_token Comma = Just Comma
  get_token _  = Nothing


dotToken :: Parsec [Token] st Token
dotToken = tokenPrim show update_pos get_token where
  get_token Dot = Just Dot
  get_token _  = Nothing
