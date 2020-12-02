module MainGrammar where

import Lexer
import Text.Parsec

import FlowPrimTokens
import CommandsPrimTokens
import LiteralsPrimTokens
import MainPrimTokens
import TypesPrimTokens
import OperatorsPrimTokens
import ScopesPrimTokens

import ExprTokenUtils

import MemTable
import SubProgTable
import TypesTable

import OurState
import OurType


import ExprExecUtils
import BasicExecUtils

import Control.Monad.IO.Class


type PParser = ParsecT [Token] OurState IO(Type, [Token])
type TokenParser = ParsecT [Token] OurState IO (Token)

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------


-- <program> -> { <import> } <blocks> // IMPORTS EH OPICIONAL
programParser :: ParsecT [Token] OurState IO([Token])
programParser = (do   imports <- many importParser
                      blocks <- blocksParser
                      eof
                      return (concat(imports) ++ blocks))


-- <import> -> HASHTAG IMPORT STRING_LIT {SEPARATOR}
importParser :: ParsecT [Token] OurState IO([Token])
importParser = (do  hashtag <- hashtagToken
                    importt <- importToken
                    fileName <- stringLitToken
                    sep <- separatorToken
                    return (hashtag:importt:fileName:[sep]))


--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------


-- <blocks> -> { <block> }
blocksParser :: ParsecT [Token] OurState IO([Token])
blocksParser = (do  blocks <- many blockParser
                    return (concat(blocks)))


-- <block> -> <stmt> SEPARATOR | <enclosed_blocks>
blockParser :: ParsecT [Token] OurState IO([Token])
blockParser = (do   stmt <- stmtParser
                    sep <- separatorToken
                    return (stmt ++ [sep])) <|>
              (do   compoundStmt <- controlStructureParser <|> subprogramsParser <|> structParser
                    return (compoundStmt)) <|>
              (do   s <- updateAndGetState (addToScope "") -- entrando num novo escopo
                    enclosedBlocks <- enclosedBlocksParser
                    s <- updateAndGetState removeFromScope -- saindo do escopo criado
                    return enclosedBlocks)


-- <enclosed_blocks> -> LEFT_BRACE <blocks> RIGHT_BRACE
enclosedBlocksParser :: ParsecT [Token] OurState IO([Token])
enclosedBlocksParser = (do  leftBrace <- leftBraceToken
                            blocks <- blocksParser
                            rightBrace <- rightBraceToken
                            return (leftBrace:blocks ++ [rightBrace]))


--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------


-- <stmt> -> CONTINUE | BREAK | <return> | <void_command> | <declrs> | <deref_pointer> <assignments_op> | <stmt_id>
stmtParser :: ParsecT [Token] OurState IO([Token])
stmtParser = (do  x <- continueToken <|> breakToken
                  return [x]) <|>
             (do  x <- returnParser <|> voidCommandParser <|> stmtIdParser
                  return x) <|>
             (do  derefPointer <- derefPointerParser
                  assignmentsOp <- assignmentsOpParser derefPointer
                  return (derefPointer ++ assignmentsOp)) <|>
             (do  (_, declrs) <- declrsParser
                  return (declrs))


-- <stmt_id> -> ID ([<index_op>] <assignments_op> | <funcall_op>)
stmtIdParser :: ParsecT [Token] OurState IO([Token])
stmtIdParser = (do  idd <- idToken -- aqui eh escrita
                    assignmentOrFuncall <- (do  maybeIndex <- (return []) -- TODO
                                                assignment <- assignmentsOpParser (idd:maybeIndex)
                                                return (maybeIndex ++ assignment)) <|>
                                            (do (_, tokens) <- funcallOpParser idd
                                                return tokens)
                    return (idd:assignmentOrFuncall))


--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------


-- <struct> -> STRUCT TYPE_ID LEFT_BRACE { <declrs> }+ RIGHT_BRACE
structParser :: ParsecT [Token] OurState IO([Token])
structParser = (do  struct <- structToken
                    idd <- typeIdToken
                    leftBrace <- leftBraceToken
                    s <- getState
                    -- se exec tiver on, vou desliga-lo pra processar as declrs
                    -- e liga-lo novamente. entao, insiro a struct criada na tabela
                    -- de tipos. se exec tiver off (vai estar off? kk) so processo os
                    -- tokens
                    if (isExecOn s) then do
                      s <- updateAndGetState turnExecOff
                      (declrs, declrsTokens) <- multipleDeclrsParser
                      s <- updateAndGetState turnExecOn
                      rightBrace <- rightBraceToken
                      s <- updateAndGetState (typesTable INSERT (StructType (getStringFromId idd, declrs)))
                      return (struct:idd:leftBrace:declrsTokens ++ [rightBrace])
                    else do
                      (declrs, declrsTokens) <- multipleDeclrsParser
                      rightBrace <- rightBraceToken
                      return (struct:idd:leftBrace:declrsTokens ++ [rightBrace]))


--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
{-
Os subprogramas serao DECLARADOS com a flag EM_EXEC DESATIVADA
e serao EXECUTADOS com a flag EM_EXEC ATIVADA

Na declaracao, o bloco de tokens do subprograma tambem sera salvo na tabela de funcoes
para futura execucao:
  Quando um subpr for declarado, serao salvos:
    seu protocolo
    seu nome
    seu retorno (se presente)
    seus comandos (bloco de execucao)
      esse como uma lista de Tokens
-}

-- <subprograms> -> <func> | <proc>
subprogramsParser :: ParsecT [Token] OurState IO([Token])
subprogramsParser = (do   x <- funcParser <|> procParser
                          return x)


-- <func> -> FUNC <type> ID <enclosed_args> <enclosed_blocks>
funcParser :: ParsecT [Token] OurState IO([Token])
funcParser =  (do   func <- funcToken
                    (semanType, typee) <- typeParser
                    idd <- idToken -- aqui eh token mesmo
                    leftParen <- leftParenToken
                    (argsSeman, args) <- argsParser
                    rightParen <- rightParenToken

                    s <- getState

                    if (isExecOn s) then do
                      s <- updateAndGetState turnExecOff
                      enclosedBlocks <- enclosedBlocksParser
                      s <- updateAndGetState turnExecOn
                      s <- updateAndGetState (subProgTable INSERT (getStringFromId idd, semanType, argsSeman, enclosedBlocks))
                      return (func:typee ++ idd:leftParen:args ++ rightParen:enclosedBlocks)
                    else do
                      enclosedBlocks <- enclosedBlocksParser
                      s <- updateAndGetState (subProgTable INSERT (getStringFromId idd, semanType, argsSeman, enclosedBlocks))
                      return (func:typee ++ idd:leftParen:args ++ rightParen:enclosedBlocks))




-- <proc> -> PROC ID <enclosed_args> <enclosed_blocks>
procParser :: ParsecT [Token] OurState IO([Token])
procParser =  (do   procc <- procToken
                    idd <- idToken -- aqui eh token mesmo
                    leftParen <- leftParenToken
                    (argsSeman, args) <- argsParser
                    rightParen <- rightParenToken

                    s <- getState

                    if (isExecOn s) then do
                      s <- updateAndGetState turnExecOff
                      enclosedBlocks <- enclosedBlocksParser
                      s <- updateAndGetState turnExecOn
                      s <- updateAndGetState (subProgTable INSERT (getStringFromId idd, NULL, argsSeman, enclosedBlocks))
                      return (procc:idd:leftParen:args ++ rightParen:enclosedBlocks)
                    else do
                      enclosedBlocks <- enclosedBlocksParser
                      s <- updateAndGetState (subProgTable INSERT (getStringFromId idd, NULL, argsSeman, enclosedBlocks))
                      return (procc:idd:leftParen:args ++ rightParen:enclosedBlocks))








--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------


-- <control_structures> -> <if> | <while> | <for>
controlStructureParser :: ParsecT [Token] OurState IO([Token])
controlStructureParser =  (do   x <- whileParser <|> forParser <|> ifParser
                                return (x))



-- <while> -> WHILE <enclosed_expr> <enclosed_blocks>
whileParser :: ParsecT [Token] OurState IO([Token])
whileParser =   (do   while <- whileToken
                      (_, enclosedExpr) <- enclosedExprParser
                      enclosedBlock <- enclosedBlocksParser
                      return (while:enclosedExpr ++ enclosedBlock))


-- <for> -> FOR LEFT_PAREN [ ( <declr> | <assignment> ) ] SEMICOLON [ <expr> ] SEMICOLON [ ( <declr> | <assignment> ) ] RIGHT_PAREN <enclosed_blocks>
forParser :: ParsecT [Token] OurState IO([Token])
forParser =   (do   for <- forToken
                    leftParen <- leftParenToken
                    maybeVarBinding1 <- (do   (_, declrs) <- declrsParser
                                              return (declrs)) <|>
                                              assignmentsParser <|>
                                              (return [])
                    semicolon <- separatorToken
                    maybeExpr <- (do  (_, x) <- exprParser
                                      return x) <|>
                                (return [])
                    semicolon <- separatorToken
                    maybeVarBinding2 <- (do   (_, declrs) <- declrsParser
                                              return (declrs)) <|>
                                              assignmentsParser <|>
                                              (return [])
                    rightParen <- rightParenToken
                    enclosedBlock <- enclosedBlocksParser
                    return (for:leftParen:maybeVarBinding1 ++ semicolon:maybeExpr ++ semicolon:maybeVarBinding2 ++ rightParen:enclosedBlock))


-- <if> -> IF <enclosed_expr> THEN <enclosed_blocks> [ ELSE ( <if> | <enclosed_blocks> ) ]
ifParser :: ParsecT [Token] OurState IO([Token])
ifParser =  (do   iff <- ifToken
                  (exprValue, enclosedExpr) <- enclosedExprParser
                  thenn <- thenToken
                  s <- getState
                  s <- updateAndGetState (addToScope "if")
                  if (not (isExecOn s)) then do
                    enclosedBlock <- enclosedBlocksParser
                    s <- updateAndGetState removeFromScope
                    maybeElse <- maybeElseParser
                    return (iff:enclosedExpr ++ thenn:enclosedBlock ++ maybeElse)
                  else do
                    if getBoolValue exprValue then do
                      enclosedBlock <- enclosedBlocksParser
                      s <- updateAndGetState removeFromScope
                      s <- updateAndGetState turnExecOff
                      maybeElse <- maybeElseParser
                      s <- updateAndGetState turnExecOn
                      return (iff:enclosedExpr ++ thenn:enclosedBlock ++ maybeElse)
                    else do
                      s <- updateAndGetState turnExecOff
                      enclosedBlock <- enclosedBlocksParser
                      s <- updateAndGetState removeFromScope
                      s <- updateAndGetState turnExecOn
                      maybeElse <- maybeElseParser
                      return (iff:enclosedExpr ++ thenn:enclosedBlock ++ maybeElse))


maybeElseParser :: ParsecT [Token] OurState IO([Token])
maybeElseParser = (do   s <- updateAndGetState (addToScope "if")
                        elsee <- elseToken
                        ifOrEnclosed <- ifParser <|> enclosedBlocksParser
                        s <- updateAndGetState removeFromScope
                        return (elsee:ifOrEnclosed)) <|>
                  (return [])


----------------------------------------------------------------------------------
----------------------------------------------------------------------------------
----------------------------------------------------------------------------------
----------------------------------------------------------------------------------



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
                maybeAccess <- (return []) -- TODO
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

                                                      s <- getState

                                                      result <- remainingExprParser (ruleParser, opParser, (eval x op y (isExecOn s)))
                                                      return (result)) <|> (return x)


remainingExprParserRight :: (PParser, TokenParser, (Type, [Token])) -> ParsecT [Token] OurState IO (Type, [Token])
remainingExprParserRight (ruleParser, opParser, x) = (do  op <- opParser
                                                          y <- ruleParser -- retorna lista de tokens e futuramente um Type
                                                          result <- remainingExprParserRight (ruleParser, opParser, y)
                                                          s <- getState
                                                          return (eval x op y (isExecOn s))) <|> (return x)


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


valueIdOpParser :: Token -> ParsecT [Token] OurState IO(Type, [Token])
valueIdOpParser idd = (do   (returnedVal, tokens) <- funcallOpParser idd
                            return (returnedVal, tokens)) <|>
                      (do   (modifiers, tokens) <- accessModifierOpParser
                            s <- getState

                            -- liftIO (print (getStringFromId idd, getScope s, NULL))

                            return (getValFromValAndModifiers (getValFromState (getStringFromId idd, getScope s, NULL) s) modifiers,
                                     tokens))



-- <value_id> -> ID [(<index_op> | <funcall>)]
valueIdParser :: ParsecT [Token] OurState IO(Type, [Token])
valueIdParser = (do   idd <- idToken
                      (val, tokens) <- (valueIdOpParser idd)
                      -- liftIO(print val)
                      return (val, idd:tokens))


-- <access_modf_op> -> (LEFT_BRACKET <expr> RIGHT_BRACKET | DOT id) <access_modf_op> | NULL
accessModifierOpParser :: ParsecT [Token] OurState IO([AccessModifier], [Token])
accessModifierOpParser = (do  leftBracket <- leftBracketToken
                              (valExpr, tokensExpr) <- exprParser
                              rightBracket <- rightBracketToken
                              (modifiers, tokensRemaining) <- accessModifierOpParser

                              return (((ArrayAM (getIntFromType valExpr)):modifiers), leftBracket:tokensExpr ++ rightBracket:tokensRemaining)) <|>
                         (do  dot <- dotToken
                              idd <- idToken
                              (modifiers, tokensRemaining) <- accessModifierOpParser

                              return (((StructAM (getStringFromId idd)):modifiers), dot:[idd])) <|>
                         (return ([],[]))


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


-- vai processar enclosed blocks
processSubProgParser :: ParsecT [Token] OurState IO (Type)
processSubProgParser = (do  _ <- enclosedBlocksParser
                            s <- getState
                            x <- (return (getValFromState ((getStringFromSubprCounter s), heapScope, NULL) s))
                            -- liftIO (print ((getStringFromSubprCounter s), heapScope, NULL))
                            -- liftIO (print (s))
                            -- liftIO (print (x))
                            s <- updateAndGetState (memTable REMOVE ((getStringFromSubprCounter s), heapScope, NULL))
                            return x)


-- <funcall_op> -> LEFT_PARENT <funcall_args> RIGHT_PARENT
funcallOpParser :: Token -> ParsecT [Token] OurState IO(Type, [Token])
funcallOpParser idd = (do   leftParen <- leftParenToken
                            (valArgs, tokenArgs) <- (do   (valExpr, tokensExpr) <- exprParser
                                                          (tailVals, tailTokens) <- funcallArgsParser
                                                          return ((valExpr:tailVals), tokensExpr ++ tailTokens)) <|>
                                                    (return ([], []))
                            rightParen <- rightParenToken


                            s <- getState
                            if (isExecOn s) then do
                              (retType, argsList, body) <- (return (searchForSubprogFromState (getStringFromId idd) s))

                              s <- updateAndGetState (addToScope (getStringFromId idd))

                              s <- updateAndGetState incrActiveSubprCounter

                              -- liftIO ( print s)
                              s <- updateAndGetState (declareArgs (getScope s) argsList valArgs)
                              -- liftIO ( print s)

                              remainingTokens <- getInput
                              setInput (body ++ remainingTokens)

                              returnedVal <- processSubProgParser -- CHAMADA

                              s <- updateAndGetState decrActiveSubprCounter

                              s <- updateAndGetState removeFromScope
                              s <- updateAndGetState turnExecOn
                              return (returnedVal, leftParen:tokenArgs ++ [rightParen])
                            else do return (NULL, leftParen:tokenArgs ++ [rightParen]))



funcallArgsParser :: ParsecT [Token] OurState IO([Type], [Token])
funcallArgsParser = (do   comma <- commaToken
                          (valExpr, tokensExpr) <- exprParser
                          (tailVals, tailTokens) <- funcallArgsParser
                          return ((valExpr:tailVals), comma:tokensExpr ++ tailTokens)) <|>
                    (return ([], []))


-- <return> -> RETURN [ <expr> ]
returnParser :: ParsecT [Token] OurState IO([Token])
returnParser = (do  ret <- returnToken
                    (valExpr, tokenExpr) <- exprParser <|> (return (NULL, []))

                    -- liftIO(print valExpr)

                    s <- getState
                    if (isExecOn s) then do
                      s <- updateAndGetState (memTable INSERT ((getStringFromSubprCounter s), heapScope, valExpr))
                      s <- updateAndGetState turnExecOff
                      return (ret:tokenExpr)
                    else do return (ret:tokenExpr))




-- <args> -> <type> ID { COMMA <type> ID } | LAMBDA
argsParser :: ParsecT [Token] OurState IO([(String, Type)], [Token])
argsParser = (do  (semanType, tokenType) <- typeParser
                  idd <- idToken -- aqui eh token mesmo
                  (remainingArgs, remainingArgsTokens) <- remainingArgsParser
                  return (((getStringFromId idd, semanType):remainingArgs, tokenType ++ idd:remainingArgsTokens))) <|>
              (return ([], []))


remainingArgsParser :: ParsecT [Token] OurState IO([(String, Type)], [Token])
remainingArgsParser = (do   comma <- commaToken
                            (semanType, tokenType) <- typeParser
                            idd <- idToken -- aqui eh token mesmo
                            (tailArgs, tailTokenArgs) <- remainingArgsParser
                            return (((getStringFromId idd, semanType):tailArgs), comma:tokenType ++ idd:tailTokenArgs)) <|>
                      (return ([], []))

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
                    s <- updateAndGetState (memTable INSERT ("", "heap", semanType))

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



--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- <void_command> -> <free> | <print> | <read>
voidCommandParser :: ParsecT [Token] OurState IO([Token])
voidCommandParser = (do   x <- freeParser <|> printParser <|> readParser
                          return x)


-- <free> -> FREE (<id> | <deref_pointer>)
freeParser :: ParsecT [Token] OurState IO([Token])
freeParser = (do  free <- freeToken
                  (exprVal, exprTokens) <- exprParser
                  -- exprVal tem que ser do tipo PointerType(tipo, id, escopo)
                  -- se nao for da merda...
                  -- remover (id, escopo) de memtable
                  s <- updateAndGetState(memTable REMOVE (getAddrFromPointer exprVal))
                  return (free:exprTokens))


-- <print> -> PRINT LEFT_PAREN <expr> RIGHT_PAREN
printParser :: ParsecT [Token] OurState IO([Token])
printParser = (do printt <- printToken
                  leftParen <- leftParenToken
                  (val, expr) <- exprParser
                  rightParen <- rightParenToken


                  s <- getState
                  if isExecOn s then do
                      liftIO (print ">>>> PRINTPARSER")
                      liftIO (print s)
                      liftIO (print val)
                      liftIO (print "<<<< PRINTPARSER")
                  else pure ()

                  return (printt:leftParen:expr ++ [rightParen]))


-- <read> -> READ LEFT_PAREN <id> RIGHT_PAREN
readParser :: ParsecT [Token] OurState IO([Token])
readParser = (do  readd <- readToken
                  leftParen <- leftParenToken
                  (idd:val) <- idParser <|> derefPointerParser -- TODO
                  rightParen <- rightParenToken

                  readVal <- liftIO (getLine)
                  s <- getState

                  s <- updateAndGetState(memTable UPDATE (getStringFromId idd, getScope s, convertStringToType readVal (getValFromState (getStringFromId idd, getScope s, NULL) s)))

                  return (readd:leftParen:val ++ [rightParen]))



---------------------------------------------------------------------------------
---------------------------------------------------------------------------------
---------------------------------------------------------------------------------

-- <assign_expr> -> ASSIGN <expr>
assignExprParser :: ParsecT [Token] OurState IO(Type, [Token])
assignExprParser = (do  assign <- assignToken
                        (val, expr) <- exprParser
                        return (val, (assign:expr)))


-- <assignments> -> <assignment> <remaining_assign>
assignmentsParser :: ParsecT [Token] OurState IO([Token])
assignmentsParser = (do   assignment <- assignmentParser
                          remaining <- remainingAssignsParser
                          return (assignment ++ remaining))


-- <assignments_op> -> <assign_expr> <remaining_assign>
assignmentsOpParser :: [Token] -> ParsecT [Token] OurState IO([Token])
assignmentsOpParser (idd:prefix) = (do  (exprVal, assignExpr) <- assignExprParser

                                        s <- getState
                                        s <- updateAndGetState(memTable UPDATE (getStringFromId idd, getScope s, exprVal))

                                        remaining <- remainingAssignsParser

                                        return (assignExpr ++ remaining))


-- <assignment> -> (<id> | <deref_pointer>) <assign_expr>
assignmentParser :: ParsecT [Token] OurState IO([Token])
assignmentParser = (do  (idd:x) <- idParser <|> derefPointerParser
                        (exprVal, assignExpr) <- assignExprParser

                        s <- getState
                        s <- updateAndGetState(memTable UPDATE (getStringFromId idd, getScope s, exprVal))

                        return ((idd:x) ++ assignExpr))


-- <remaining_assign> -> { COMMA <assignment> }
remainingAssignsParser :: ParsecT [Token] OurState IO([Token])
remainingAssignsParser = (do  remaining <- many (do   comma <- commaToken
                                                      assignment <- assignmentParser
                                                      return (comma:assignment))
                              return (concat(remaining)))


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


-- <declrs> -> <type> <maybe_assigned_id>  { COMMA <maybe_assigned_id>}]
declrsParser :: ParsecT [Token] OurState IO([(String, Type)], [Token])
declrsParser = (do  (semanType, typee) <- typeParser
                    idd <- idToken -- aqui eh tokens mesmo
                    (val, maybeAssignExpr) <- assignExprParser <|> (return (NULL, []))

                    (tailDeclr, tokens) <- remainingDeclrsParser semanType

                    -- esse parser pode ser acessado como um statement ou como
                    -- um struct. quando acessado, pode ser que a flag de execucao
                    -- esteja desligada (struct, declaracao de funcao) ou ligada
                    -- (execucao normal). quando a flag estiver desligada, nao iremos
                    -- fazer o update das tabelas de memoria.
                    -- em ambos os casos vamos retornar uma lista de (String, Type)
                    -- a fim de poder usar esses valores na construcao de um struct

                    -- pode ser que VAL == NULL: a variavel inicial nao foi inicializada

                    s <- getState
                    if isExecOn s then do -- checando se flag de execucao ta on
                      if val == NULL then do
                        s <- updateAndGetState(memTable INSERT (getStringFromId idd, getScope s, semanType))
                        return (((getStringFromId idd, semanType):tailDeclr), (typee ++ idd:maybeAssignExpr ++ tokens))
                      else do
                        s <- updateAndGetState(memTable INSERT (getStringFromId idd, getScope s, val))
                        return (((getStringFromId idd, val):tailDeclr), (typee ++ idd:maybeAssignExpr ++ tokens))
                    else do
                      if val == NULL then do
                        return (((getStringFromId idd, semanType):tailDeclr), (typee ++ idd:maybeAssignExpr ++ tokens))
                      else do
                        return (((getStringFromId idd, val):tailDeclr), (typee ++ idd:maybeAssignExpr ++ tokens)))


-- <declrs> -> {<declr> SEPARATOR}+
multipleDeclrsParser :: ParsecT [Token] OurState IO([(String, Type)], [Token])
multipleDeclrsParser = (do  (hDeclrs, hTokens) <- declrsParser
                            x <- separatorToken
                            (tDeclrs, tTokens) <- multipleDeclrsParser <|> (return ([], []))
                            return (hDeclrs ++ tDeclrs, hTokens ++ x:tTokens))


-- <maybe_assigned_id> -> ID [<assign_expr>]
remainingDeclrsParser :: Type -> ParsecT [Token] OurState IO([(String, Type)], [Token])
remainingDeclrsParser typee = (do   comma <- commaToken
                                    idd <- idToken -- aqui eh token mesmo
                                    (val, maybeAssignExpr) <- assignExprParser <|> (return (NULL, []))

                                    -- pode ser que VAL == NULL: a variavel nao foi inicializada

                                    (vals, tokens) <- remainingDeclrsParser typee
                                    s <- getState
                                    if val == NULL then do
                                      s <- updateAndGetState (memTable INSERT (getStringFromId idd, getScope s, typee))
                                      return ((getStringFromId idd, typee):vals, comma:idd:maybeAssignExpr ++ tokens)
                                    else do
                                      s <- updateAndGetState (memTable INSERT (getStringFromId idd, getScope s, val))
                                      return ((getStringFromId idd, val):vals, comma:idd:maybeAssignExpr ++ tokens)) <|>
                              (return ([], []))
