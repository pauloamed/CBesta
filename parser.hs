module Main (main) where

import Lexer
import Text.Parsec
import Control.Monad.IO.Class

import System.IO.Unsafe

import MainGrammar

-- invocação do parser para o símbolo de partida
parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT programParser (([], 0), ([], 0), [], "$", True) "Error message" tokens

main :: IO ()
-- main = print((getTokens "ultimate.cb"))
main =  case unsafePerformIO( parser (getTokens "examples/Probleminhas.cb")) of
            { Left err -> print err;
              Right ans -> print (id "DONE")
              -- Right ans -> print ans
            }
