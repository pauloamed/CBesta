module Grammar where

import Lexer
import Text.Parsec

import FlowPrimTokens
import LiteralsPrimTokens
import MainPrimTokens
import OperatorsPrimTokens
import ScopesPrimTokens


-- <program> -> { <import> } { <subpgrogram> }+
programParser :: Parsec [Token] st [Token]
programParser = do
            imports <- many importParser
            subprograms <- many1 subprogramParser
            eof
            return ((concat imports) ++ (concat subprograms))


-- <import> -> HASHTAG IMPORT LESS_THAN NAME GREATER_THAN
importParser :: Parsec [Token] st [Token]
importParser = do
            hashtag <- hashtagToken
            importt <- importToken
            lessThan <- lessThanToken
            fileName <- idToken -- ?
            greaterThan <- greaterThanToken
            return (hashtag:importt:lessThan:fileName:[greaterThan])


-- <subprogram> -> <func> | <proc> | <block>
subprogramParser :: Parsec [Token] st [Token]
subprogramParser = (do  func <- funcParser
                        return func
                ) <|> (do   procc <- procParser
                            return procc
                ) <|> (do   block <- blockParser
                            return block
                )


-- <func> -> FUNC TYPE ID <aux_args> <aux_blocks>
funcParser :: Parsec [Token] st [Token]
funcParser = (do
            func <- funcToken
            typee <- typeToken
            idd <- idToken
            auxArgs <- auxArgsParser
            auxBlock <- auxBlockParser
            return (func:typee:idd:auxArgs ++ auxBlock))


-- <proc> -> PROC TYPE ID <aux_args> <aux_blocks>
procParser :: Parsec [Token] st [Token]
procParser = do
            procc <- procToken
            typee <- typeToken
            idd <- idToken
            auxArgs <- auxArgsParser
            auxBlock <- auxBlockParser
            return (procc:typee:idd:auxArgs ++ auxBlock)


-- <aux_args> -> LEFT_PAREN [ <args> ] RIGHT_PAREN
auxArgsParser :: Parsec [Token] st [Token]
auxArgsParser = do
            leftParen <- leftParenToken
            args <- argsParser
            rightParen <- rightParenToken
            return (leftParen:args ++ [rightParen])


-- <aux_block> -> LEFT_PAREN [ <args> ] RIGHT_PAREN
auxBlockParser :: Parsec [Token] st [Token]
auxBlockParser = do
            leftBrace <- leftBraceToken
            block <- blockParser
            rightBrace <- rightBraceToken
            return (leftBrace:block ++ [rightBrace])


-- <args> -> TYPE ID { COMMA TYPE ID } | LAMBDA
argsParser :: Parsec [Token] st [Token]
argsParser = (do  typee <- typeToken
                  idd <- idToken
                  remainingArgs <- many argParser
                  return typee:idd:(concat remainingArgs)) <|>
                (return [])


argParser :: Parsec [Token] st [Token]
argParser = do
            comma <- commaToken
            typee <- typeToken
            idd <- idToken
            return commaToken:typeToken:[idToken]


blockParser :: Parsec [Token] st [Token]
blockParser = do
            prefixBlock <- prefixBlockParser
            blocks <- many blockParser
            return prefixBlock ++ (concat blocks)


prefixBlockParser :: Parsec [Token] st [Token]
prefixBlockParser = (do   stmt <- stmtParser
                          separator <- separatorParser
                          return stmt ++ separator
                    ) <|> (return auxBlockParser
                    ) <|> (return controlStructureParser
                    )


separatorParser :: Parsec [Token] st [Token]
separatorParser = (return semicolonToken) <|> (return newLineToken)


stmtParser :: Parsec [Token] st [Token]
stmtParser = (do  a <- lessThanToken
                  return a)


controlStructureParser :: Parsec [Token] st [Token]
controlStructureParser = (do  a <- lessThanToken
                              return a:[])

--
-- sub :: Parsec [Token] st [Token]
-- stmts = do
--           first <- assign
--           next <- remaining_stmts
--           return (first ++ next)
--
-- assign :: Parsec [Token] st [Token]
-- assign = do
--           a <- idToken
--           b <- assignToken
--           c <- intToken
--           return (a:b:[c])
--
-- remaining_stmts :: Parsec [Token] st [Token]
-- remaining_stmts = (do a <- semiColonToken
--                       b <- assign
--                       return (a:b)) <|> (return [])
