module ExprGrammar where

import Lexer
import Text.Parsec

import FlowPrimTokens
import LiteralsPrimTokens
import MainPrimTokens
import OperatorsPrimTokens
import ScopesPrimTokens

-- <enclosed_expr> -> LEFT_PAREN <expr> RIGHT_PAREN
enclosedExprParser :: Parsec [Token] st [Token]
enclosedExprParser = (do  leftParen <- leftParenToken
                          expr <- exprParser
                          rightParen <- rightParenToken
                          return leftParen:expr ++ [rightParen])


-- <expr> -> <raw_expr> | <cast> <raw_expr>
exprParser :: Parsec [Token] st [Token]
exprParser = (do  rawExpr <- rawExprParser
                  return rawExpr) <|>
             (do  cast <- castingParser
                  rawExpr <- rawExprParser
                  return cast ++ rawExpr)


-- <raw_expr> -> <un_op> <expr> | <enclosed_expr> | <raw_expr_term>
rawExprParser :: Parsec [Token] st [Token]
rawExprParser = (do   unop <- unopParser
                      expr <- exprParser
                      return (unop ++ expr)) <|>
                (do   enclosedExpr <- enclosedExprParser
                      return enclosedExpr) <|>
                (do   rawExprTerm <- rawExprTermParser
                      return rawExprTerm)


-- <raw_expr_term> -> <term> (<bin_op> <expr> | LAMBDA)
rawExprTermParser :: Parsec [Token] st [Token]
rawExprTermParser = (do   term <- termParser
                          binopOrLambda <- (do  binop <- binopParser
                                                expr <- exprParser
                                                return (binop ++ expr)) <|>
                                           (return [])
                          return term ++ binopOrLambda)


-- <un_op> -> NEGATION | MINUS
unopParser :: Parsec [Token] st [Token]
unopParser =  (do   neg <- negationToken
                    return ([neg])) <|>
              (do   minus <- minusToken
                    return ([minus]))


-- <bin_op> -> <arit_op> | <bool_op>
binopParser :: Parsec [Token] st [Token]
binopParser = (do   aritOp <- aritOpParser
                    return (aritOp)) <|>
              (do   boolOp <- boolOpParser
                    return (boolOp))


-- <arit_op> -> MINUS | PLUS | STAR | EXPO | DIV | MOD
aritOpParser :: Parsec [Token] st [Token]
aritOpParser =  (do   x <- minusToken
                      return ([x])) <|>
                (do   x <- plusToken
                      return ([x])) <|>
                (do   x <- starToken
                      return ([x])) <|>
                (do   x <- expoToken
                      return ([x])) <|>
                (do   x <- divToken
                      return ([x])) <|>
                (do   x <- modToken
                      return ([x]))


-- <bool_op> -> LESSTHAN | GREATERTHAN | LESSEQUALS | GREATEREQUALS | EQUALS | DIFFERENCE
boolOpParser :: Parsec [Token] st [Token]
boolOpParser =  (do   x <- lessThanToken
                      return ([x])) <|>
                (do   x <- greaterThanToken
                      return ([x])) <|>
                (do   x <- lessEqualsToken
                      return ([x])) <|>
                (do   x <- greaterEqualsToken
                      return ([x])) <|>
                (do   x <- equalsToken
                      return ([x])) <|>
                (do   x <- differenceToken
                      return ([x])) <|>
                (do   x <- andToken
                      return ([x])) <|>
                (do   x <- orToken
                      return ([x]))


-- <casting> -> LEFT_BRACKET <type> RIGHT_BRACKET
castingParser :: Parsec [Token] st [Token]
castingParser = (do   leftBracket <- leftBracketParser
                      typee <- typeParser
                      rightBracket <- rightBrackterParser
                      return leftBrackter:typee ++ [rightBracket])


-- <term> ->  <deref_pointer> | <literal> | <command_with_ret> | <term_id> | STRING <split_op>
termParser :: Parsec [Token] st [Token]
termParser = (do  derefPointer <- derefPointerParser -- tem que importar da main
                  return derefPointer) <|>
             (do  literal <- literalParser
                  return literal) <|>
             (do  command <- commandWithRetParser
                  return command) <|>
             (do  termId <- termIdParser
                  return termId) <|>
             (do  strLit <- stringLitToken
                  splitOp <- splitOpParser
                  return strLit:splitOp)


-- <literal> -> INT_LIT | BOOL_LIT | DOUBLE_LIT | STRING_LIT
literalParser :: Parsec [Token] st [Token]
literalParser = (do   int <- intLitToken
                      return [int]) <|>
                (do   bool <- boolLitToken
                      return [bool]) <|>
                (do   double <- doubleLitToken
                      return [double]) <|>
                (do   string <- stringLitToken
                      return [string])


-- <command_with_ret> -> <alloc> | <addr> | <len> | <size_of>
commandWithRetParser :: Parsec [Token] st [Token]
commandWithRetParser = (do  x <- allocParser
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
