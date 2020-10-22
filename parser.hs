module Main (main) where

import Lexer
import Text.Parsec

import Grammar

-- invocação do parser para o símbolo de partida
parser :: [Token] -> Either ParseError [Token]
parser tokens = runParser programParser () "Error message" tokens

main :: IO ()
main =  case parser (getTokens "programa.cb") of
            { Left err -> print err;
              Right ans -> print ans
            }
