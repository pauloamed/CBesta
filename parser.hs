module Main (main) where

import Lexer
import Text.Parsec

import MainGrammar

-- invocação do parser para o símbolo de partida
parser :: [Token] -> Either ParseError [Token]
parser tokens = runParser programParser () "Error message" tokens

main :: IO ()
-- main = print((getTokens "ultimate.cb"))
main =  case parser (getTokens "ultimate.cb") of
            { Left err -> print err;
              Right ans -> print ans
            }
