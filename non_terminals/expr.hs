module ExprGrammar where

import Lexer
import Text.Parsec

import FlowPrimTokens
import LiteralsPrimTokens
import MainPrimTokens
import OperatorsPrimTokens
import ScopesPrimTokens

-- <enclosed_expr> -> LEFT_PAREN <expr> RIGHT_PAREN
-- <expr> -> <raw_expr> | <cast> <raw_expr>
-- <raw_expr> -> <un_op> <expr> | <enclosed_expr> | <raw_expr_term>
-- <raw_expr_term> -> <term> (<bin_op> <expr> | LAMBDA)
--
-- <un_op> -> NEGATION | MINUS
-- <bin_op> -> <arit_op> | <bool_op>
-- <arit_op> -> MINUS | PLUS | STAR | EXPO | DIV | MOD
-- <bool_op> -> LESSTHAN | GREATERTHAN | LESSEQUALS | GREATEREQUALS | EQUALS | DIFFERENCE
--
-- <casting> -> LEFT_BRACKET <type> RIGHT_BRACKET
--
-- <term> ->  <deref_pointer> | <literal> | <command_with_ret> | <term_id> | STRING <split_op>
--
-- <literal> -> INT_LIT | BOOL_LIT | DOUBLE_LIT | STRING_LIT
--
-- <ret_command> -> <alloc> | <addr> | <len> | <size_of>
retCommand :: Parsec [Token] st [Token]
retCommand = (do  x <- allocParser
                  return x) <|>
             (do  x <- addrParser
                  return x) <|>
             (do  x <- lenParser
                  return x) <|>
             (do  x <- sizeOfParser
                  return x)

-- <alloc> -> ALLOC <enclosed_expr>
allocParser :: Parsec [Token] st [Token]
allocParser = (do   alloc <- allocToken
                    enclosedExpr <- enclosedExprParser
                    return alloc:enclosedExpr)

-- <addr> -> ADDR LEFT_PAREN ID RIGHT_PAREN
addrParser :: Parsec [Token] st [Token]
addrParser = ( do addr <- addrToken
                  leftParen <- leftParenToken
                  idd <- idToken
                  rightParen <- rightParenToken
                  return addr:leftParen:idd:[rightParen])

-- <len> -> LEN LEFT_PAREN ID RIGHT_PAREN
lenParser :: Parsec [Token] st [Token]
lenParser = ( do  len <- lenToken
                  leftParen <- leftParenToken
                  idd <- idToken
                  rightParen <- rightParenToken
                  return len:leftParen:idd:[rightParen])

-- <size_of> -> SIZEOF LEFT_PAREN <type> RIGHT_PAREN
sizeOfParser :: Parsec [Token] st [Token]
sizeOfParser = (  do  sizeOf <- sizeOfToken
                      leftParen <- leftParenToken
                      typee <- typeParser
                      rightParen <- rightParenToken
                      return sizeOf:leftParen:typee ++ [rightParen])


-- <term_id> -> ID (<funcall_op> | <split_op>)
termIdParser :: Parsec [Token] st [Token]
termIdParser =  (do idd <- idToken
                    funcallOpOrSplitOp <- ( do funcallOp <- funcallOpParser
                                            return funcallOp) <|>
                                          ( do splitOp <- splitOpParser
                                            return splitOp)
                    return idd:funcallOpOrSplitOp)

-- <split_op> -> LEFT_BRACKET [ <expr> ] COLON [ <expr> ] RIGHT_BRACKET
splitOpParser :: Parsec [Token] st [Token]
splitOpParser =   (do   leftBracket <- leftBracketToken
                        maybeExpr1 <- ( do   expr <- exprParser
                                        return expr) <|> (return [])
                        colon <- colonToken
                        maybeExpr1 <- ( do   expr <- exprParser
                                        return expr) <|> (return [])
                        rightBracket <- rightBracketToken
                        return (leftBracket:maybeExpr1 ++ [colon] ++ maybeExpr1 ++ [rightBracket]))
