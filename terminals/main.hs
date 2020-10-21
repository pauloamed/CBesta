module MainPrimTokens where


import Lexer
import Text.Parsec
import TerminalUtils


typeToken :: Parsec [Token] st Token
typeToken = tokenPrim show update_pos get_token where
  get_token (Type p x) = Just (Type p x)
  get_token _  = Nothing


idToken :: Parsec [Token] st Token
idToken = tokenPrim show update_pos get_token where
  get_token (Id p x) = Just (Id p x)
  get_token _  = Nothing


returnToken :: Parsec [Token] st Token
returnToken = tokenPrim show update_pos get_token where
  get_token (Return p)  = Just (Return p)
  get_token _  = Nothing


importToken :: Parsec [Token] st Token
importToken = tokenPrim show update_pos get_token where
  get_token (Import p)  = Just (Import p)
  get_token _  = Nothing


mainToken :: Parsec [Token] st Token
mainToken = tokenPrim show update_pos get_token where
  get_token (Main p)  = Just (Main p)
  get_token _  = Nothing


funcToken :: Parsec [Token] st Token
funcToken = tokenPrim show update_pos get_token where
  get_token (Func p)  = Just (Func p)
  get_token _  = Nothing


procToken :: Parsec [Token] st Token
procToken = tokenPrim show update_pos get_token where
  get_token (Proc p)  = Just (Proc p)
  get_token _  = Nothing


hashtagToken :: Parsec [Token] st Token
hashtagToken = tokenPrim show update_pos get_token where
  get_token (Hashtag p)  = Just (Hashtag p)
  get_token _  = Nothing


separatorToken :: Parsec [Token] st Token
separatorToken = tokenPrim show update_pos get_token where
  get_token (Separator p)  = Just (Separator p)
  get_token _  = Nothing


colonToken :: Parsec [Token] st Token
colonToken = tokenPrim show update_pos get_token where
  get_token (Colon p)  = Just (Colon p)
  get_token _  = Nothing


commaToken :: Parsec [Token] st Token
commaToken = tokenPrim show update_pos get_token where
  get_token (Comma p)  = Just (Comma p)
  get_token _  = Nothing


dotToken :: Parsec [Token] st Token
dotToken = tokenPrim show update_pos get_token where
  get_token (Dot p)  = Just (Dot p)
  get_token _  = Nothing
