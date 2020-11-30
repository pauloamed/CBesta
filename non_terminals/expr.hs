 module ExprGrammar where

import Lexer
import Text.Parsec

import FlowPrimTokens
import LiteralsPrimTokens
import MainPrimTokens
import OperatorsPrimTokens
import ScopesPrimTokens
import TypesPrimTokens
import CommandsPrimTokens

import MemTable
import SubProgTable
import TypesTable

import OurState
import OurType

import ExprExecUtils
import BasicExecUtils

import ExprTokenUtils

import Control.Monad.IO.Class

type PParser = ParsecT [Token] OurState IO(Type, [Token])
type TokenParser = ParsecT [Token] OurState IO (Token)


-- <type> -> POINTER <enclosed_type>
--           | ARRAY LESS_THAN <expr> COMMA <type> GREATER_THAN
--           | INT | BOOL | DOUBLE | STRING
--           | TYPE_ID
typeParser :: ParsecT [Token] OurState IO (Type, [Token])
typeParser = (do  simpleTypeToken <- intToken <|> boolToken <|> doubleToken <|> stringToken
                  return (createSimpleType simpleTypeToken, [simpleTypeToken])) <|>
             (do  idd <- typeIdToken
                  s <- getState
                  return (getTypeFromState (getStringFromId idd) s, [idd])) <|>
             (do  array <- arrayToken
                  lessThan <- lessThanToken
                  (arraySize, size) <- exprParser
                  comma <- commaToken
                  (semanType, typeToken) <- typeParser
                  greaterThan <- greaterThanToken
                  return (createArray arraySize semanType, (array:lessThan:typeToken ++ comma:size ++ [greaterThan]))) <|>
             (do  x <- pointerToken
                  lessThan <- lessThanToken
                  (semanType, typeToken) <- typeParser
                  greaterThan <- greaterThanToken
                  return (createPointer semanType, x:lessThan:typeToken ++ [greaterThan]))


-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------


-- <id> -> ID [<index>]
idParser :: ParsecT [Token] OurState IO([Token])
idParser = (do  idd <- idToken
                maybeAccess <- indexOpParser <|> (return [])
                return (idd:maybeAccess))


-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------



-- <enclosed_expr> -> LEFT_PAREN <expr> RIGHT_PAREN
enclosedExprParser :: ParsecT [Token] OurState IO (Type, [Token])
enclosedExprParser = (do  leftParen <- leftParenToken
                          (val, expr) <- exprParser
                          rightParen <- rightParenToken
                          return (val, leftParen:expr ++ [rightParen]))


remainingExprParser :: (PParser, TokenParser, (Type, [Token])) -> ParsecT [Token] OurState IO (Type, [Token])
remainingExprParser (ruleParser, opParser, x) = (do   op <- opParser
                                                      y <- ruleParser -- retorna lista de tokens e futuramente um Type
                                                      result <- remainingExprParser (ruleParser, opParser, (eval x op y))
                                                      return (result)) <|> (return x)


remainingExprParserRight :: (PParser, TokenParser, (Type, [Token])) -> ParsecT [Token] OurState IO (Type, [Token])
remainingExprParserRight (ruleParser, opParser, x) = (do  op <- opParser
                                                          y <- ruleParser -- retorna lista de tokens e futuramente um Type
                                                          result <- remainingExprParserRight (ruleParser, opParser, y)
                                                          return (eval x op y)) <|> (return x)


-- <expr> -> <expr_7> <remaining_expr>(<expr7>, OR)
exprParser :: ParsecT [Token] OurState IO (Type, [Token])
exprParser = (do  expr7 <- expr7Parser
                  result <- remainingExprParser (expr7Parser, orToken, expr7)
                  return (result))


-- <expr_7> -> <expr_6> <remaining_expr>(<expr6>, AND)
expr7Parser :: ParsecT [Token] OurState IO(Type, [Token])
expr7Parser = (do   expr6 <- expr6Parser
                    result <- remainingExprParser (expr6Parser, andToken, expr6)
                    return (result))



-- <expr_6> -> <expr_5> <remaining_expr>(<expr5>, (EQUALS, DIFF))
expr6Parser :: ParsecT [Token] OurState IO(Type, [Token])
expr6Parser = (do   expr5 <- expr5Parser
                    result <- remainingExprParser (expr5Parser, expr6OpParser, expr5)
                    return (result))


-- <expr_5> -> <expr_4> <remaining_expr>(<expr4>, (GREATER | LESS | GREATER_EQ | LESS_EQ))
expr5Parser :: ParsecT [Token] OurState IO(Type, [Token])
expr5Parser = (do   expr4 <- expr4Parser
                    result <- remainingExprParser (expr4Parser, expr5OpParser, expr4)
                    return (result))


-- <expr_4> -> <expr_3> <remaining_expr>(<expr3>, (PLUS | MINUS))
expr4Parser :: ParsecT [Token] OurState IO(Type, [Token])
expr4Parser = (do   expr3 <- expr3Parser
                    result <- remainingExprParser (expr3Parser, expr4OpParser, expr3)
                    return (result))


-- <expr_3> -> <expr_2> <remaining_expr>(<expr2>, (TIMES | DIV | MOD))
expr3Parser :: ParsecT [Token] OurState IO(Type, [Token])
expr3Parser = (do   expr2 <- expr2Parser
                    result <- remainingExprParser (expr2Parser, expr3OpParser, expr2)
                    return (result))


-- <expr_2> -> [(MINUS | NEG)] <expr_1>
expr2Parser :: ParsecT [Token] OurState IO(Type, [Token])
expr2Parser = (do   unop <- negationToken <|> minusToken
                    (x, expr1) <- expr1Parser
                    return (evalUnopType unop x, [unop] ++ expr1)) <|>
              (do   (x, expr1) <- expr1Parser
                    return (x, expr1))


-- <expr_1> -> <value> [ EXP <expr_2>]
expr1Parser :: ParsecT [Token] OurState IO (Type, [Token])
expr1Parser = (do   value <- valueParser
                    result <- remainingExprParserRight (expr2Parser, expoToken, value)
                    return (result))


-- <value> ->  <deref_pointer> | <literal> | <command_with_ret> | <value_id> | <enclosed_expr>
valueParser :: ParsecT [Token] OurState IO (Type, [Token])
valueParser = (do   literal <- literalParser
                    return literal) <|>
              (do   valueId <- valueIdParser
                    return valueId) <|>
              (do   encExpr <- enclosedExprParser
                    return encExpr) <|>
              (do   command <- commandWithRetParser
                    return command)
              -- (do   derefPointer <- derefPointerParser
              --       return derefPointer) <|>


-- <literal> -> INT_LIT | BOOL_LIT | DOUBLE_LIT | STRING_LIT
literalParser :: ParsecT [Token] OurState IO(Type, [Token])
literalParser = (do   x <- intLitToken <|> boolLitToken <|> doubleLitToken <|> stringLitToken
                      return (getLiteralType x, [x]))


--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------


-- <value_id> -> ID [(<index_op> | <funcall>)]
valueIdParser :: ParsecT [Token] OurState IO(Type, [Token])
valueIdParser = (do   idd <- idToken
                      -- funcallOpOrIndexOp <- funcallOpParser <|> indexOpParser <|> (return [])
                      funcallOpOrIndexOp <- (return [])
                      s <- getState

                      return (getValFromState (getStringFromId idd, getScope s, NULL) s,(idd:funcallOpOrIndexOp)))


-- <index_op> -> LEFT_BRACKET <expr> RIGHT_BRACKET [<index_op>]
indexOpParser :: ParsecT [Token] OurState IO([Token])
indexOpParser = (do   leftBracket <- leftBracketToken
                      (_, expr) <- exprParser
                      rightBracket <- rightBracketToken
                      remaining <- indexOpParser <|> (return [])
                      return (leftBracket:expr ++ rightBracket:remaining))


--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------


-- <deref_pointer> -> STAR <id>
derefPointerParser :: ParsecT [Token] OurState IO([Token])
derefPointerParser = (do  star <- starToken
                          (idd:x) <- idParser
                          -- id parser eh um ponteiro
                          -- quero olhar o valor
                          return (star:(idd:x)))


--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------


-- <funcall> -> ID <funcall_op>
funcallParser :: ParsecT [Token] OurState IO([Token])
funcallParser = (do   idd <- idToken
                      funcallOp <- funcallOpParser
                      return (idd:funcallOp))


-- <funcall_op> -> LEFT_PARENT <funcall_args> RIGHT_PARENT
funcallOpParser :: ParsecT [Token] OurState IO([Token])
funcallOpParser = (do   leftParen <- leftParenToken
                        funcallArgs <- (do  (_, expr) <- exprParser
                                            remaining <- many (do   comma <- commaToken
                                                                    (_, expr) <- exprParser
                                                                    return (comma:expr))
                                            return (expr++concat(remaining))) <|>
                                       (return [])
                        rightParen <- rightParenToken
                        return (leftParen:funcallArgs ++ [rightParen]))



--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------

-- <command_with_ret> -> <alloc> | <addr> | <len> | <cast> | <substr>
commandWithRetParser :: ParsecT [Token] OurState IO(Type, [Token])
commandWithRetParser = (do  x <- subStrParser <|> castParser <|> lenParser <|> allocParser <|> addrParser
                            return x)


-- <cast> -> CAST LEFT_PAREN <expr> COMMA <type> RIGHT_PAREN
castParser :: ParsecT [Token] OurState IO(Type, [Token])
castParser = (do  castT <- castToken
                  lp <- leftParenToken
                  (exprVal, expr) <- exprParser
                  c <- commaToken
                  (semanType, t) <- typeParser
                  rp <- rightParenToken
                  return (cast exprVal semanType , castT:lp:expr ++ c:t ++ [rp]))


-- <alloc> -> ALLOC <type>
allocParser :: ParsecT [Token] OurState IO(Type, [Token])
allocParser = (do   alloc <- allocToken
                    lp <- leftParenToken
                    (semanType, tokenType) <- typeParser
                    rp <- rightParenToken

                    s <- getState
                    updateState (memTable INSERT ("", "heap", semanType))

                    return (getAlloc s semanType, alloc:lp:tokenType ++ [rp]))


-- <addr> -> ADDR LEFT_PAREN <id> RIGHT_PAREN
addrParser :: ParsecT [Token] OurState IO(Type, [Token])
addrParser = ( do addr <- addrToken
                  leftParen <- leftParenToken
                  (idd:x) <- idParser
                  rightParen <- rightParenToken

                  s <- getState

                  return (getAddrFromIdFromState (getStringFromId idd) s, addr:leftParen:(idd:x) ++ [rightParen]))


-- <len> -> LEN LEFT_PAREN <id> RIGHT_PAREN
lenParser :: ParsecT [Token] OurState IO(Type, [Token])
lenParser = ( do  len <- lenToken
                  leftParen <- leftParenToken
                  (exprVal, expr) <- exprParser
                  rightParen <- rightParenToken

                  s <- getState
                  return (getLen exprVal, len:leftParen:expr ++ [rightParen]))


--  subst(string, ini, excl)
-- <substr> -> SUBSTR LEFT_PAREN <expr> COMMA <expr> COMMA <expr> RIGHT_PAREN
subStrParser :: ParsecT [Token] OurState IO(Type, [Token])
subStrParser = (do  substr <- substrToken
                    lp <- leftParenToken
                    (strVal, str) <- exprParser
                    c1 <- commaToken
                    (leftVal, left) <- exprParser
                    c2 <- commaToken
                    (rightVal, right) <- exprParser
                    rp <- rightParenToken


                    return (getSubstr leftVal rightVal strVal, substr:lp:str ++ c1:left ++ c2:right ++ [rp]))
