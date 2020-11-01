module ExprGrammar where

import Lexer
import Text.Parsec

import FlowPrimTokens
import LiteralsPrimTokens
import MainPrimTokens
import OperatorsPrimTokens
import ScopesPrimTokens


import TypeGrammar


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
unopParser =  (do   x <- negationToken <|> minusToken
                    return ([x]))


-- <bin_op> -> <arit_op> | <bool_op>
binopParser :: Parsec [Token] st [Token]
binopParser = (do   x <- aritOpParser <|> boolOpParser
                    return (x))


-- <arit_op> -> MINUS | PLUS | STAR | EXPO | DIV | MOD
aritOpParser :: Parsec [Token] st [Token]
aritOpParser =  (do   x <- minusToken <|> plusToken <|> starToken <|> expoToken <|> divToken <|> modToken
                      return ([x]))

-- <bool_op> -> LESSTHAN | GREATERTHAN | LESSEQUALS | GREATEREQUALS | EQUALS | DIFFERENCE
boolOpParser :: Parsec [Token] st [Token]
boolOpParser =  (do   x <- lessThanToken <|> greaterThanToken <|> lessEqualsToken <|> greaterEqualsToken <|> equalsToken <|> differenceToken <|> andToken <|> orToken
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
literalParser = (do   x <- intLitToken <|> boolLitToken <|> doubleLitToken <|> stringLitToken
                      return [x])


-- <command_with_ret> -> <alloc> | <addr> | <len> | <size_of>
commandWithRetParser :: Parsec [Token] st [Token]
commandWithRetParser = (do  x <- allocParser <|> addrParser <|> lenParser <|> sizeOfParser
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
                        maybeExpr1 <- exprParser <|> (return [])
                        colon <- colonToken
                        maybeExpr1 <- ( do   expr <- exprParser
                                        return expr) <|> (return [])
                        rightBracket <- rightBracketToken
                        return (leftBracket:maybeExpr1 ++ [colon] ++ maybeExpr1 ++ [rightBracket]))


-- <deref_pointer> -> STAR ID
derefPointerParser :: Parsec [Token] st [Token]
derefPointerParser = (do  star <- starToken
                          idd <- idToken
                          return start:[idd])


-- <funcall> -> ID <funcall_op>
funcallParser :: Parsec [Token] st [Token]
funcallParser = (do   idd <- idToken
                      funcallOp <- funcallOpParser
                      return idd:funcallOp)


-- <funcall_op> -> LEFT_PARENT <funcall_args> RIGHT_PARENT
funcallOp :: Parsec [Token] st [Token]
funcallOp = (do   leftParen <- leftParenToken
                  funcallArgs <- funcallArgsParser
                  rightParen <- rightParenToken
                  return leftParen:funcallArgs ++ [rightParen])


-- <funcall_args> -> <expr> { COMMA <expr> } | LAMBDA
funcallArgsParser :: Parsec [Token] st [Token]
funcallArgsParser = (do   expr <- exprParser
                          remeaning <- many (do   comma <- commaToken
                                                  expr <- exprParser
                                                  return (comma:expr))
                          return expr++concat(remeaning)) <|> (return [])
