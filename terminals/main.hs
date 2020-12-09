module MainPrimTokens where


import Lexer
import Text.Parsec
import TerminalUtils


idToken :: ParsecT [Token] st IO (Token)
idToken = tokenPrim show update_pos get_token where
  get_token (Id p x) = Just (Id p x)
  get_token _  = Nothing


typeIdToken :: ParsecT [Token] st IO (Token)
typeIdToken = tokenPrim show update_pos get_token where
  get_token (TypeId p x) = Just (TypeId p x)
  get_token _  = Nothing


returnToken :: ParsecT [Token] st IO (Token)
returnToken = tokenPrim show update_pos get_token where
  get_token (Return p)  = Just (Return p)
  get_token _  = Nothing


funcToken :: ParsecT [Token] st IO (Token)
funcToken = tokenPrim show update_pos get_token where
  get_token (Func p)  = Just (Func p)
  get_token _  = Nothing


procToken :: ParsecT [Token] st IO (Token)
procToken = tokenPrim show update_pos get_token where
  get_token (Proc p)  = Just (Proc p)
  get_token _  = Nothing


hashtagToken :: ParsecT [Token] st IO (Token)
hashtagToken = tokenPrim show update_pos get_token where
  get_token (Hashtag p)  = Just (Hashtag p)
  get_token _  = Nothing


separatorToken :: ParsecT [Token] st IO (Token)
separatorToken = tokenPrim show update_pos get_token where
  get_token (Separator p)  = Just (Separator p)
  get_token _  = Nothing


colonToken :: ParsecT [Token] st IO (Token)
colonToken = tokenPrim show update_pos get_token where
  get_token (Colon p)  = Just (Colon p)
  get_token _  = Nothing


commaToken :: ParsecT [Token] st IO (Token)
commaToken = tokenPrim show update_pos get_token where
  get_token (Comma p)  = Just (Comma p)
  get_token _  = Nothing


dotToken :: ParsecT [Token] st IO (Token)
dotToken = tokenPrim show update_pos get_token where
  get_token (Dot p)  = Just (Dot p)
  get_token _  = Nothing


structToken :: ParsecT [Token] st IO (Token)
structToken = tokenPrim show update_pos get_token where
  get_token (Struct p)  = Just (Struct p)
  get_token _  = Nothing
