module Main (main) where

import Lexer
import Text.Parsec

import Grammar

-- invocação do parser para o símbolo de partida
parser :: [Token] -> Either ParseError [Token]
parser tokens = runParser programParser () "Error message" tokens

main :: IO ()
-- main = print((getTokens "programa.cb"))
main =  case parser (getTokens "programa.cbestasoh") of
            { Left err -> print err;
              Right ans -> print ans
            }
