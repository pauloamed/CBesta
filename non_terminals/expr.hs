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



-- <type> -> POINTER <enclosed_type>
--           | ARRAY LESS_THAN <expr> COMMA <type> GREATER_THAN
--           | TUPLE LESS_THAN <types> GREATER_THAN
--           | INT | BOOL | DOUBLE | STRING
--           | TYPE_ID
typeParser :: Parsec [Token] st [Token]
typeParser = (do  x <- pointerToken
                  lessThan <- lessThanToken
                  typee <- typeParser
                  greaterThan <- greaterThanToken
                  return (x:lessThan:typee ++ [greaterThan])) <|>
             (do  simpleType <- intToken <|> boolToken <|> doubleToken <|> stringToken
                  return [simpleType]) <|>
             (do  array <- arrayToken
                  lessThan <- lessThanToken
                  size <- exprParser
                  comma <- commaToken
                  typee <- typeParser
                  greaterThan <- greaterThanToken
                  return (array:lessThan:typee ++ comma:size ++ [greaterThan])) <|>
             (do  tuple <- tupleToken
                  lessThan <- lessThanToken
                  typee <- typeParser
                  remaining <- many1 (do   comma <- commaToken
                                           typee <- typeParser
                                           return (comma:typee))
                  greaterThan <- greaterThanToken
                  return (tuple:lessThan:typee ++ concat(remaining) ++ [greaterThan])) <|>
             (do  idd <- typeIdToken
                  return ([idd]))


-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------



-- <id> -> ID [<index>]
idParser :: Parsec [Token] st [Token]
idParser = (do  idd <- idToken
                maybeAccess <- indexOpParser <|> (return [])
                return (idd:maybeAccess))


-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------



-- <enclosed_expr> -> LEFT_PAREN <expr> RIGHT_PAREN
enclosedExprParser :: Parsec [Token] st [Token]
enclosedExprParser = (do  leftParen <- leftParenToken
                          expr <- exprParser
                          rightParen <- rightParenToken
                          return (leftParen:expr ++ [rightParen]))


-- <expr> -> <expr_7> [ OR <expr>]
exprParser :: Parsec [Token] st [Token]
exprParser = (do  expr7 <- expr7Parser
                  remainingExpr <- (do  t <- orToken
                                        expr <- exprParser
                                        return (t:expr)) <|>
                                   (return [])
                  return (expr7 ++ remainingExpr))


-- <expr_7> -> <expr_6> [ AND <expr_7>]
expr7Parser :: Parsec [Token] st [Token]
expr7Parser = (do   expr6 <- expr6Parser
                    remainingExpr <- (do  t <- andToken
                                          expr7 <- expr7Parser
                                          return (t:expr7)) <|>
                                     (return [])
                    return (expr6 ++ remainingExpr))


-- <expr_6> -> <expr_5> [(EQUALS | DIF) <expr_6>]
expr6Parser :: Parsec [Token] st [Token]
expr6Parser = (do   expr5 <- expr5Parser
                    remainingExpr <- (do  t <- equalsToken <|> differenceToken
                                          expr6 <- expr6Parser
                                          return (t:expr6)) <|>
                                     (return [])
                    return (expr5 ++ remainingExpr))


-- <expr_5> -> <expr_4> [(GREATER | LESS | GREATER_EQ | LESS_EQ) <expr_5>]
expr5Parser :: Parsec [Token] st [Token]
expr5Parser = (do   expr4 <- expr4Parser
                    remainingExpr <- (do  t <- lessThanToken <|> greaterThanToken <|> lessEqualsToken <|> greaterEqualsToken
                                          expr5 <- expr5Parser
                                          return (t:expr5)) <|>
                                     (return [])
                    return (expr4 ++ remainingExpr))


-- <expr_4> -> <expr_3> [(PLUS | MINUS) <expr_4>]
expr4Parser :: Parsec [Token] st [Token]
expr4Parser = (do   expr3 <- expr3Parser
                    remainingExpr <- (do  t <- plusToken <|> minusToken
                                          expr4 <- expr4Parser
                                          return (t:expr4)) <|>
                                     (return [])
                    return (expr3 ++ remainingExpr))


-- <expr_3> -> <expr_2> [(TIMES | DIV | MOD) <expr_3>]
expr3Parser :: Parsec [Token] st [Token]
expr3Parser = (do   expr2 <- expr2Parser
                    remainingExpr <- (do  t <- starToken <|> divToken <|> modToken
                                          expr3 <- expr3Parser
                                          return (t:expr3)) <|>
                                     (return [])
                    return (expr2 ++ remainingExpr))


-- <expr_2> -> [(MINUS | NEG)] <expr_1>
expr2Parser :: Parsec [Token] st [Token]
expr2Parser = (do   unop <- (do   x <- negationToken <|> minusToken
                                  return ([x])) <|>
                            (return [])
                    expr1 <- expr1Parser
                    return (unop ++ expr1))


-- <expr_1> -> <value> [ EXP <expr_2>]
expr1Parser :: Parsec [Token] st [Token]
expr1Parser = (do   value <- valueParser
                    remainingExpr <- (do  t <- expoToken
                                          expr2 <- expr2Parser
                                          return (t:expr2)) <|>
                                     (return [])
                    return (value ++ remainingExpr))


-- <un_op> -> NEGATION | MINUS
unopParser :: Parsec [Token] st [Token]
unopParser =  (do   x <- negationToken <|> minusToken
                    return ([x]))


-- <value> ->  <deref_pointer> | <literal> | <command_with_ret> | <value_id> | <enclosed_expr>
valueParser :: Parsec [Token] st [Token]
valueParser = (do   derefPointer <- derefPointerParser
                    return derefPointer) <|>
              (do   literal <- literalParser
                    return literal) <|>
              (do   command <- commandWithRetParser
                    return command) <|>
              (do   value <- valueIdParser
                    return value) <|>
              (do   encExpr <- enclosedExprParser
                    return encExpr)


-- <literal> -> INT_LIT | BOOL_LIT | DOUBLE_LIT | STRING_LIT
literalParser :: Parsec [Token] st [Token]
literalParser = (do   x <- intLitToken <|> boolLitToken <|> doubleLitToken <|> stringLitToken
                      return [x])


--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------


-- <value_id> -> ID [(<index_op> | <funcall>)]
valueIdParser :: Parsec [Token] st [Token]
valueIdParser = (do   idd <- idToken
                      funcallOpOrSplitOp <- funcallOpParser <|> indexOpParser <|> (return [])
                      return (idd:funcallOpOrSplitOp))


-- <index_op> -> LEFT_BRACKET <expr> RIGHT_BRACKET [<index>]
indexOpParser :: Parsec [Token] st [Token]
indexOpParser = (do   leftBracket <- leftBracketToken
                      expr <- exprParser
                      rightBracket <- rightBracketToken
                      remaining <- indexOpParser <|> (return [])
                      return (leftBracket:expr ++ rightBracket:remaining))


--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------


-- <deref_pointer> -> STAR <id>
derefPointerParser :: Parsec [Token] st [Token]
derefPointerParser = (do  star <- starToken
                          idd <- idParser
                          return (star:idd))


--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------


-- <funcall> -> ID <funcall_op>
funcallParser :: Parsec [Token] st [Token]
funcallParser = (do   idd <- idToken
                      funcallOp <- funcallOpParser
                      return (idd:funcallOp))


-- <funcall_op> -> LEFT_PARENT <funcall_args> RIGHT_PARENT
funcallOpParser :: Parsec [Token] st [Token]
funcallOpParser = (do   leftParen <- leftParenToken
                        funcallArgs <- (do  expr <- exprParser
                                            remaining <- many (do   comma <- commaToken
                                                                    expr <- exprParser
                                                                    return (comma:expr))
                                            return (expr++concat(remaining))) <|>
                                       (return [])
                        rightParen <- rightParenToken
                        return (leftParen:funcallArgs ++ [rightParen]))



--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------

-- <command_with_ret> -> <alloc> | <addr> | <len> | <cast> | <substr>
commandWithRetParser :: Parsec [Token] st [Token]
commandWithRetParser = (do  x <- allocParser <|> addrParser <|> lenParser <|> castParser <|> subStrParser
                            return x)


-- <cast> -> CAST LEFT_PAREN <expr> COMMA <type> RIGHT_PAREN
castParser :: Parsec [Token] st [Token]
castParser = (do  cast <- castToken
                  lp <- leftParenToken
                  expr <- exprParser
                  c <- commaToken
                  t <- typeParser
                  rp <- rightParenToken
                  return (cast:lp:expr ++ c:t ++ [rp]))


-- <alloc> -> ALLOC <type>
allocParser :: Parsec [Token] st [Token]
allocParser = (do   alloc <- allocToken
                    lp <- leftParenToken
                    typee <- typeParser
                    rp <- rightParenToken
                    return (alloc:lp:typee ++ [rp]))


-- <addr> -> ADDR LEFT_PAREN <id> RIGHT_PAREN
addrParser :: Parsec [Token] st [Token]
addrParser = ( do addr <- addrToken
                  leftParen <- leftParenToken
                  idd <- idParser
                  rightParen <- rightParenToken
                  return (addr:leftParen:idd ++ [rightParen]))


-- <len> -> LEN LEFT_PAREN <id> RIGHT_PAREN
lenParser :: Parsec [Token] st [Token]
lenParser = ( do  len <- lenToken
                  leftParen <- leftParenToken
                  idd <- idParser
                  rightParen <- rightParenToken
                  return (len:leftParen:idd ++ [rightParen]))


-- <substr> -> SUBSTR LEFT_PAREN <expr> COMMA <expr> COMMA <expr> RIGHT_PAREN
subStrParser :: Parsec [Token] st [Token]
subStrParser = (do  substr <- substrToken
                    lp <- leftParenToken
                    str <- exprParser
                    c1 <- commaToken
                    left <- exprParser
                    c2 <- commaToken
                    right <- exprParser
                    rp <- rightParenToken
                    return (substr:lp:str ++ c1:left ++ c2:right ++ [rp]))
