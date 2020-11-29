module ExprTokenUtils where

import Lexer
import Text.Parsec

import OperatorsPrimTokens

import OurState


-- parsers auxs de operacoes para cada nivel de expressao

expr6OpParser :: ParsecT [Token] OurState IO (Token)
expr6OpParser = try equalsToken <|> differenceToken


expr5OpParser :: ParsecT [Token] OurState IO (Token)
expr5OpParser = try lessThanToken <|> greaterThanToken <|> lessEqualsToken <|> greaterEqualsToken


expr4OpParser :: ParsecT [Token] OurState IO (Token)
expr4OpParser = try plusToken <|> minusToken


expr3OpParser :: ParsecT [Token] OurState IO (Token)
expr3OpParser = try starToken <|> divToken <|> modToken
