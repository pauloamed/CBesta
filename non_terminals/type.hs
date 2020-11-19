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
--           | ARRAY LESS_THAN <type> COMMA INT_LIT GREATER_THAN
--           | TUPLE LESS_THAN <types> GREATER_THAN
--           | <simple_type>
--           | TYPE_ID
typeParser :: Parsec [Token] st [Token]
typeParser = (do  x <- pointerToken
                  enclosedType <- enclosedTypeParser
                  return (x:enclosedType)) <|>
             (do  simpleType <- simpleTypeParser
                  return simpleType) <|>
             (do  array <- arrayToken
                  lessThan <- lessThanToken
                  typee <- typeParser
                  comma <- commaToken
                  size <- intParser
                  greaterThan <- greaterThanToken
                  return (array:lessThan:typee ++ comma:size ++ [greaterThan])) <|>
             (do  tuple <- tupleToken
                  lessThan <- lessThanToken
                  types <- typesParser
                  greaterThan <- greaterThanToken
                  return (tuple:lessThan:types ++ [greaterThan])) <|>
             (do  idd <- typeIdToken
                  return ([idd]))


-- <int_parser> -> (INT_LIT, ID)
intParser :: Parsec [Token] st [Token]
intParser = (do   x <- (do  y <- intLitToken
                            return ([y])) <|>
                        idParser
                  return x)


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


-- <id> -> ID [LEFT_BRACKET <int_parser> RIGHT_BRACKET]
idParser :: Parsec [Token] st [Token]
idParser = (do  idd <- idToken
                maybeAccess <- (do  leftBracket <- leftBracketToken
                                    intt <- intParser
                                    rightBracket <- rightBracketToken
                                    return (leftBracket:intt ++ [rightBracket])) <|>
                                    (return [])
                return (idd:maybeAccess))
