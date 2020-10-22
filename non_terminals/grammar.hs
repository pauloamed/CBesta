module Grammar where

import Lexer
import Text.Parsec

import FlowPrimTokens
import LiteralsPrimTokens
import MainPrimTokens
import OperatorsPrimTokens
import ScopesPrimTokens



-- <program> -> { <import> } { <subpgrogram> }
programParser :: Parsec [Token] st [Token]
programParser = do
            imports <- many importParser
            subprograms <- many subprogramParser
            eof
            return ((concat imports) ++ (concat subprograms))


-- <import> -> HASHTAG IMPORT NAME
importParser :: Parsec [Token] st [Token]
importParser = do
            hashtag <- hashtagToken
            importt <- importToken
            fileName <- stringToken
            sep <- many1 separatorParser
            return (hashtag:importt:[fileName])


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


-- <proc> -> PROC ID <aux_args> <aux_blocks>
procParser :: Parsec [Token] st [Token]
procParser =  (do   procc <- procToken
                    idd <- idToken
                    auxArgs <- auxArgsParser
                    auxBlock <- auxBlockParser
                    return (procc:idd:auxArgs ++ auxBlock))


-- <aux_args> -> LEFT_PAREN <args> RIGHT_PAREN
auxArgsParser :: Parsec [Token] st [Token]
auxArgsParser = (do   leftParen <- leftParenToken
                      args <- argsParser
                      rightParen <- rightParenToken
                      return (leftParen:args ++ [rightParen]))


-- <aux_block> -> LEFT_BRACE <maybe_block> RIGHT_BRACE
auxBlockParser :: Parsec [Token] st [Token]
auxBlockParser = (do  leftBrace <- leftBraceToken
                      maybeBlock <- maybeBlockParser
                      rightBrace <- rightBraceToken
                      return (leftBrace:maybeBlock ++ [rightBrace]))


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


-- <maybe_block> -> <block> | LAMBDA
maybeBlockParser :: Parsec [Token] st [Token]
maybeBlockParser =  (do   block <- blockParser
                          return (block))  <|>
                    (return [])


-- <prefix_block> -> <stmt> <separator> | <aux_block> | <ctrl_structure>
prefixBlockParser :: Parsec [Token] st [Token]
prefixBlockParser = (do   stmt <- stmtParser
                          separator <- separatorParser
                          return (stmt)) <|>
                    (do   controlStructure <- controlStructureParser
                          return (controlStructure)) <|>
                    (do   auxBlock <- auxBlockParser
                          return (auxBlock))


-- <separator> -> SEPARATOR
separatorParser :: Parsec [Token] st [Token]
separatorParser =   (do   separator <- many1 separatorToken
                          return ([]))


-- <stmt> -> CONTINUE | BREAK | <return> | <declr> | <expr_or_assignment>
stmtParser :: Parsec [Token] st [Token]
stmtParser =  (do continue <- continueToken
                  return ([continue])) <|>
              (do breakk <- breakToken
                  return ([breakk])) <|>
              (do returnn <- returnParser
                  return (returnn)) <|>
              (do declr <- declrParser
                  return (declr)) <|>
              (do exprOrAssign <- exprOrAssignParser
                  return (exprOrAssign))


-- <assign> -> ID ASSIGN <expr>
assignParser :: Parsec [Token] st [Token]
assignParser = (do  idd <- idToken
                    assign <- assignToken
                    expr <- exprParser
                    return (idd:assign:expr))


-- <expr_or_assign> -> ID <maybe_funcall_or_assign> | <literal> <maybe_binop> | LEFT_PARENT <expr> RIGHT_PARENT <maybe_binop> | <unop> <expr>
exprOrAssignParser :: Parsec [Token] st [Token]
exprOrAssignParser =  (do   idd <- idToken
                            maybeFuncallOrAssign <- maybeFuncallOrAssignParser
                            return (idd:maybeFuncallOrAssign)) <|>
                      (do   lit <- literalParser
                            maybeBinop <- maybeBinopParser
                            return (lit ++ maybeBinop)) <|>
                      (do   leftParen <- leftParenToken
                            expr <- exprParser
                            rightParen <- rightParenToken
                            maybeBinop <- maybeBinopParser
                            return (leftParen:expr ++ rightParen:maybeBinop)) <|>
                      (do   unop <- unopParser
                            expr <- exprParser
                            return (unop ++ expr))


-- <maybe_funcall_or_assign> -> <maybe_funcall> <maybe_binop> | ASSIGN <expr>
maybeFuncallOrAssignParser :: Parsec [Token] st [Token]
maybeFuncallOrAssignParser =  (do   assign <- assignToken
                                    expr <- exprParser
                                    return (assign:expr)) <|>
                              (do   maybeFuncall <- maybeFuncallParser
                                    maybeBinop <- maybeBinopParser
                                    return (maybeFuncall ++ maybeBinop))


-- <maybe_binop> -> <binop> <expr> | LAMBDA
maybeBinopParser :: Parsec [Token] st [Token]
maybeBinopParser =  (do   binop <- binopParser
                          expr <- exprParser
                          return (binop ++ expr)) <|>
                    (return [])


-- <expr> -> <term> <maybe_binop> | <unop> <expr>
exprParser :: Parsec [Token] st [Token]
exprParser =  (do   term <- termParser
                    maybeBinop <- maybeBinopParser
                    return (term ++ maybeBinop)) <|>
              (do   unop <- unopParser
                    expr <- exprParser
                    return (unop ++ expr))


-- <term> ->  <id_or_funcall> | <literal> | LEFT_PARENT <expr> RIGHT_PARENT
-- <id_or_funcall> -> ID <maybe_funcall>
termParser :: Parsec [Token] st [Token]
termParser =  (do   idd <- idToken
                    maybeFuncall <- maybeFuncallParser
                    return (idd:maybeFuncall)) <|>
              (do   lit <- literalParser
                    return (lit)) <|>
              (do   leftParen <- leftParenToken
                    expr <- exprParser
                    rightParen <- rightParenToken
                    return (leftParen:expr ++ [rightParen]))


-- <maybe_funcall> -> LEFT_PARENT <func_args> RIGHT_PARENT | LAMBDA
maybeFuncallParser :: Parsec [Token] st [Token]
maybeFuncallParser =  (do   leftParen <- leftParenToken
                            funcArgs <- funcArgsParser
                            rightParen <- rightParenToken
                            return (leftParen:funcArgs ++ [rightParen])) <|>
                      (return [])


-- <split_string> -> ( ID | STRING ) LEFT_BRACKET [ ( INT | ID ) ] COLON [ ( INT | ID ) ] RIGHT_BRACKET
splitStringParser :: Parsec [Token] st [Token]
splitStringParser = (do       idOrString <- idOrStringParser
                              leftBracket <- leftBracketToken
                              maybeIntOrId <- maybeIntOrIdParser
                              colon <- colonToken
                              maybeIntOrId <- maybeIntOrIdParser
                              rightBracket <- rightBracketToken
                              return (idOrString ++ [leftBracket] ++ maybeIntOrId ++ [colon] ++ maybeIntOrId ++ [rightBracket]))


-- <maybe_int_or_id> -> INT | ID | LAMBIDA_EM_BERTAO
maybeIntOrIdParser :: Parsec [Token] st [Token]
maybeIntOrIdParser = (do      int <- intToken
                              return ([int])) <|>
                     (do      idd <- idToken
                              return ([idd])) <|>
                     (return [])


-- <id_or_string> -> ID | STRING 
idOrStringParser :: Parsec [Token] st [Token]
idOrStringParser = (do   idd <- idToken
                         return ([idd])) <|>
                   (do   string <- stringToken
                         return ([string]))


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


-- <maybe_else> -> ELSE <if_or_aux_block> | LAMBDA
maybeElseParser :: Parsec [Token] st [Token]
maybeElseParser =   (do   elsee <- elseToken
                          ifOrAuxBlock <- ifOrAuxBlockParser
                          return (elsee:ifOrAuxBlock)) <|>
                    (return [])


-- <if_or_aux_block> -> <if> | <aux_block>
ifOrAuxBlockParser :: Parsec [Token] st [Token]
ifOrAuxBlockParser =  (do   iff <- ifParser
                            return (iff)) <|>
                      (do   auxBlock <- auxBlockParser
                            return (auxBlock))


-- <while> -> WHILE LEFT_PARENT <expr> RIGHT_PARENT <aux_block>
whileParser :: Parsec [Token] st [Token]
whileParser =   (do   while <- whileToken
                      leftParen <- leftParenToken
                      expr <- exprParser
                      rightParen <- rightParenToken
                      auxBlock <- auxBlockParser
                      return (while:leftParen:expr ++ rightParen:auxBlock))


-- <for> -> FOR LEFT_PARENT <maybe_assignment_or_declr> SEMICOLON <maybe_expr> SEMICOLON <maybe_assignment> RIGHT_PARENT <aux_block>
forParser :: Parsec [Token] st [Token]
forParser =   (do   for <- forToken
                    leftParen <- leftParenToken
                    maybeAssignOrDeclr <- maybeAssignOrDeclrParser
                    semicolon <- separatorToken
                    maybeExpr <- maybeExprParser
                    semicolon <- separatorToken
                    maybeAssign <- maybeAssignParser
                    rightParen <- rightParenToken
                    auxBlock <- auxBlockParser
                    return (for:leftParen:maybeAssignOrDeclr ++ semicolon:maybeExpr ++ semicolon:maybeAssign ++ rightParen:auxBlock))


-- <maybe_assignment_or_declr> -> <assign> | <declr> | LAMBDA
maybeAssignOrDeclrParser :: Parsec [Token] st [Token]
maybeAssignOrDeclrParser =  (do   assign <- assignParser
                                  return (assign)) <|>
                            (do   declr <- declrParser
                                  return (declr)) <|>
                            (return [])


-- <maybe_assignment> -> <assign> | LAMBDA
maybeAssignParser :: Parsec [Token] st [Token]
maybeAssignParser = (do   assign <- assignParser
                          return (assign)) <|>
                    (return [])



-- <maybe_expr> -> <expr> | LAMBDA
maybeExprParser :: Parsec [Token] st [Token]
maybeExprParser = (do   expr <- exprParser
                        return (expr)) <|>
                  (return [])


-- <return> -> RETURN <maybe_expr>
returnParser :: Parsec [Token] st [Token]
returnParser = (do  ret <- returnToken
                    maybeExpr <- maybeExprParser
                    return (ret:maybeExpr))


-- <declr> -> TYPE ID <maybe_init>
declrParser :: Parsec [Token] st [Token]
declrParser =   (do   typee <- typeToken
                      idd <- idToken
                      maybeInit <- maybeInitParser
                      return (typee:idd:maybeInit))


-- <maybe_init> -> ASSIGN <expr> | LAMBDA
maybeInitParser :: Parsec [Token] st [Token]
maybeInitParser =   (do   assign <- assignToken
                          expr <- exprParser
                          return (assign:expr)) <|>
                    (return [])


-- <literal> -> INT | BOOL | DOUBLE | STRING
literalParser :: Parsec [Token] st [Token]
literalParser = (do   int <- intToken
                      return [int]) <|>
                (do   bool <- boolToken
                      return [bool]) <|>
                (do   double <- doubleToken
                      return [double]) <|>
                (do   string <- stringToken
                      return [string])


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
                      return ([x])) <|>
                (do   x <- andToken
                      return ([x])) <|>
                (do   x <- orToken
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
