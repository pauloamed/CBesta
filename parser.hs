module Main (main) where

import Lexer
import Text.Parsec
import Control.Monad.IO.Class
import System.Environment
import System.IO
import OurState

import System.IO.Unsafe

import MainGrammar

-- invocação do parser para o símbolo de partida
parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT programParser (([], 0), [], [], [rootScope], True, 0, []) "Error message" tokens


main :: IO ()
main = (do  args <- getArgs
            case unsafePerformIO( parser (getTokens (head args))) of
                      { Left err -> print err;
                        Right ans -> print (id "")
                        -- Right ans -> print ans
                      }
          )
