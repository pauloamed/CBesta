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
            sep <- many1 separatorParser
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
auxBlockParser = (do  leftBrace <- leftBraceToken
                      block <- blockParser
                      rightBrace <- rightBraceToken
                      return (leftBrace:block ++ [rightBrace]))


-- <args> -> TYPE ID { <arg> } | LAMBDA
argsParser :: Parsec [Token] st [Token]
argsParser = (do  typee <- typeToken
                  idd <- idToken
                  remainingArgs <- many argParser
                  return (typee:idd:(concat remainingArgs))) <|>
                (return [])


-- <arg> -> COMMA TYPE ID
argParser :: Parsec [Token] st [Token]
argParser = (do   comma <- commaToken
                  typee <- typeToken
                  idd <- idToken
                  return (comma:typee:[idd]))


-- <block> -> <prefix_block> { <block> }
blockParser :: Parsec [Token] st [Token]
blockParser = (do   prefixBlock <- prefixBlockParser
                    blocks <- many blockParser
                    return (prefixBlock ++ (concat blocks)))


-- <prefix_block> -> <stmt> <separator> | <aux_block> | <ctrl_structure>
prefixBlockParser :: Parsec [Token] st [Token]
prefixBlockParser = (do   stmt <- stmtParser
                          separator <- many1 separatorParser
                          return (stmt)) <|>
                    (do   controlStructure <- controlStructureParser
                          return (controlStructure)) <|>
                    (do   auxBlock <- auxBlockParser
                          return (auxBlock))


-- <separator> -> SEPARATOR
separatorParser :: Parsec [Token] st [Token]
separatorParser =   (do   separator <- many1 separatorToken
                          return ([]))

-- <stmt> -> CONTINUE | BREAK | <return> | <assignment> |  TYPE ID | <expr>
stmtParser :: Parsec [Token] st [Token]
stmtParser =  (do continue <- continueToken
                  return ([continue])) <|>
              (do breakk <- breakToken
                  return ([breakk])) <|>
              (do returnn <- returnParser
                  return (returnn)) <|>
              (do assign <- assignParser
                  return (assign)) <|>
              (do typee <- typeToken
                  idd <- idToken
                  return (typee:[idd])) <|>
              (do expr <- exprParser
                  return (expr))

-- <ctrl_structure> -> <while> | <for> | <if>
controlStructureParser :: Parsec [Token] st [Token]
controlStructureParser =  (do   while <- whileParser
                                return (while)) <|>
                          (do   for <- forParser
                                return (for)) <|>
                          (do   iff <- ifParser
                                return (iff))


-- <if> -> IF LEFT_PAREN <expr> RIGHT_PAREN THEN <aux_block> <maybe_else>
ifParser :: Parsec [Token] st [Token]
ifParser =  (do   iff <- ifToken
                  leftParen <- leftParenToken
                  expr <- exprParser
                  rightParen <- rightParenToken
                  thenn <- thenToken
                  auxBlock <- auxBlockParser
                  maybeElse <- maybeElseParser
                  return (iff:leftParen:expr ++ rightParen:thenn:auxBlock ++ maybeElse))


-- <maybe_else> -> ELSE (<if> | <aux_block>)
maybeElseParser :: Parsec [Token] st [Token]
maybeElseParser =   (do   elsee <- elseToken
                          ifBody <- ifParser
                          return (elsee:ifBody)) <|>
                    (do   elsee <- elseToken
                          auxBlockBody <- auxBlockParser
                          return (elsee:auxBlockBody))


-- <while> -> WHILE LEFT_PARENT <expr> RIGHT_PARENT <aux_block>
whileParser :: Parsec [Token] st [Token]
whileParser =   (do   while <- whileToken
                      leftParen <- leftParenToken
                      expr <- exprParser
                      rightParen <- rightParenToken
                      auxBlock <- auxBlockParser
                      return (while:leftParen:expr ++ rightParen:auxBlock))


-- <for> -> FOR LEFT_PARENT <maybe_assignment> SEMICOLON <maybe_expr> SEMICOLON <maybe_assignment> RIGHT_PARENT <aux_block>
forParser :: Parsec [Token] st [Token]
forParser =   (do   for <- forToken
                    leftParen <- leftParenToken
                    maybeAssign <- maybeAssignParser
                    semicolon <- semicolonToken
                    maybeExpr <- maybeExprParser
                    semicolon <- semicolonToken
                    maybeAssign <- maybeAssignParser
                    rightParen <- rightParenToken
                    auxBlock <- auxBlockParser
                    return (for:leftParen:maybeAssign ++ semicolon:maybeExpr ++ semicolon:maybeAssign ++ rightParen:auxBlock))


-- <maybe_assign> -> <assign> | LAMBDA
maybeAssignParser :: Parsec [Token] st [Token]
maybeAssignParser = (do   assign <- assignParser
                          return (assign)) <|>
                    (return [])


-- <maybe_assign> -> <expr> | LAMBDA
maybeExprParser :: Parsec [Token] st [Token]
maybeExprParser = (do   expr <- exprParser
                        return (expr)) <|>
                  (return [])


-- <return> -> RETURN [ <expr> ]
returnParser :: Parsec [Token] st [Token]
returnParser = (do  ret <- returnToken
                    expr <- exprParser
                    return (ret:expr)) <|>
               (do  ret <- returnToken
                    return [ret])



-- <assign> -> [ TYPE ] ID ASSIGN <expr>
assignParser :: Parsec [Token] st [Token]
assignParser = (do  typee <- typeToken
                    idd <- idToken
                    assign <- assignToken
                    expr <- exprParser
                    return (typee:idd:assign:expr))


-- <expr> -> <term> | <term> <binop> <expr> | <unop> <expr>
exprParser :: Parsec [Token] st [Token]
exprParser =  (do   term <- termParser
                    return (term)) <|>
              (do   term <- termParser
                    binop <- binopParser
                    expr <- exprParser
                    return (term ++ binop ++ expr)) <|>
              (do   unop <- unopParser
                    expr <- exprParser
                    return (unop ++ expr))


-- <term> -> ID | <literal> | <funcall> | LEFT_PARENT <expr> RIGHT_PARENT
termParser :: Parsec [Token] st [Token]
termParser =  (do   idd <- idToken
                    return ([idd])) <|>
              (do   lit <- literalParser
                    return (lit)) <|>
              (do   funcall <- funcallParser
                    return (funcall)) <|>
              (do   leftParen <- leftParenToken
                    expr <- exprParser
                    rightParen <- rightParenToken
                    return (leftParen:expr ++ [rightParen]))


-- <literal> -> INT | BOOL | DOUBLE | STRING
literalParser :: Parsec [Token] st [Token]
literalParser = (do   int <- intToken
                      return ([int])) <|>
                (do   bool <- boolToken
                      return [bool]) <|>
                (do   double <- doubleToken
                      return [double]) <|>
                (do   string <- stringToken
                      return [string])

-- <funcall> -> ID LEFT_PARENT <func_args> RIGHT_PARENT
funcallParser :: Parsec [Token] st [Token]
funcallParser = (do   leftParen <- leftParenToken
                      funcArgs <- funcArgsParser
                      rightParen <- rightParenToken
                      return (leftParen:funcArgs ++ [rightParen]))


-- <func_args> -> <expr> { <funcArgsAuxParser> } | LAMBDA
funcArgsParser :: Parsec [Token] st [Token]
funcArgsParser =  (do   expr <- exprParser
                        funcArgsAux <- many funcArgsAuxParser
                        return (expr ++ concat (funcArgsAux))) <|>
                  (return ([]))


-- <funcArgsAuxParser> -> COMMA <expr>
funcArgsAuxParser :: Parsec [Token] st [Token]
funcArgsAuxParser = (do   comma <- commaToken
                          expr <- exprParser
                          return (comma:expr))


-- <unop> -> NEGATION | MINUS
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


-- <arit_op> MINUS | PLUS | TIMES | EXPO | DIV | MOD
aritOpParser :: Parsec [Token] st [Token]
aritOpParser =  (do   x <- minusToken
                      return ([x])) <|>
                (do   x <- plusToken
                      return ([x])) <|>
                (do   x <- timesToken
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
                      return ([x]))
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
