module TypeGrammar where

import Lexer
import Text.Parsec

import FlowPrimTokens
import LiteralsPrimTokens
import MainPrimTokens
import OperatorsPrimTokens
import ScopesPrimTokens
import TypesPrimTokens


-- <type> -> POINTER <enclosed_type>
--           | LIST <enclosed_type>
--           | ARRAY <enclosed_type>
--           | HASHMAP LESS_THAN <type> COMMA <type> GREATER_THAN
--           | TUPLE LESS_THAN <types> GREATER_THAN
--           | <simple_type>
--           | ID
typeParser :: Parsec [Token] st [Token]
typeParser = (do  x <- pointerToken <|> listToken <|> arrayToken
                  enclosedType <- enclosedTypeParser
                  return (x:enclosedType)) <|>
             (do  simpleType <- simpleTypeParser
                  return simpleType) <|>
             (do  hashmap <- hashmapToken
                  lessThan <- lessThanToken
                  typee1 <- typeParser
                  comma <- commaToken
                  typee2 <- typeParser
                  greaterThan <- greaterThanToken
                  return (hashmap:lessThan:typee1 ++ comma:typee2 ++ [greaterThan])) <|>
             (do  tuple <- tupleToken
                  lessThan <- lessThanToken
                  types <- typesParser
                  greaterThan <- greaterThanToken
                  return (tuple:lessThan:types ++ [greaterThan]))
                  -- (do  idd <- idToken
                  --      return ([idd])) <|>


-- <enclosed_type> -> LESS_THAN <type> GREATE_THAN
enclosedTypeParser :: Parsec [Token] st [Token]
enclosedTypeParser = (do  lessThan <- lessThanToken
                          typee <- typeParser
                          greaterThan <- greaterThanToken
                          return (lessThan:typee ++ [greaterThan]))


-- <types> -> <type> { COMMA <type> }
typesParser :: Parsec [Token] st [Token]
typesParser = (do   typee <- typeParser
                    remeaning <- many (do   comma <- commaToken
                                            typee <- typeParser
                                            return (comma:typee))
                    return (typee ++ concat(remeaning)))


-- <simple_type> -> INT | BOOL | DOUBLE | STRING
simpleTypeParser :: Parsec [Token] st [Token]
simpleTypeParser = (do  x <- intToken <|> boolToken <|> doubleToken <|> stringToken
                        return [x])
