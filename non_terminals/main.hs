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

import Scope

import OurState
import OurType


import ExprExecUtils
import BasicExecUtils
import CreateExecUtils 
import ModifiersReadExecUtils

import Control.Monad.IO.Class


type PParser = ParsecT [Token] OurState IO(Type, [Token])
type TokenParser = ParsecT [Token] OurState IO (Token)

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------


-- <program> -> { <import> } <blocks> // IMPORTS EH OPICIONAL
programParser :: ParsecT [Token] OurState IO([Token])
programParser = (do   imports <- many importParser
                      (_, blocks) <- blocksParser
                      eof
                      return (concat(imports) ++ blocks))


-- <import> -> HASHTAG IMPORT STRING_LIT {SEPARATOR}
importParser :: ParsecT [Token] OurState IO([Token])
importParser = (do  hashtag <- hashtagToken
                    importt <- importToken
                    fileName <- stringLitToken -- o nome do arquivo
                    sep <- separatorToken
                    return (hashtag:importt:fileName:[sep]))


--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------


-- <blocks> -> { <block> } -- vai processar uma sequencia de blocos
blocksParser :: ParsecT [Token] OurState IO(Bool, [Token])
blocksParser = (do  (hasReturn, block) <- blockParser
                    (hasReturnRemaining, blocks) <- remainingBlocksParser
                    -- retorna se tem stmt de retorno em algum de seus blocos
                    return ((hasReturn || hasReturnRemaining), (block ++ blocks)))


-- vai processar os blocos restantes de (blocksParser)
remainingBlocksParser :: ParsecT [Token] OurState IO(Bool, [Token])
remainingBlocksParser = (do   (hasReturn, block) <- blockParser
                              (hasReturnRemaining, blocks) <- remainingBlocksParser
                              -- retorna se tem stmt de retorno em algum de seus blocos
                              return ((hasReturn || hasReturnRemaining), (block ++ blocks))) <|>
                        (return (False, [])) -- caso base, nao ha mais blocos, nao ha stmt de retorno (por isso q eh False)


-- <block> -> <stmt> SEPARATOR | <enclosed_blocks> | <compound_stmts>
-- o bloco da linguagem
blockParser :: ParsecT [Token] OurState IO(Bool, [Token])
blockParser = (do   (hasReturn, stmt) <- stmtParser -- pode ser um stmt seguido de um separador
                    sep <- separatorToken
                    return (hasReturn, stmt ++ [sep])) <|>
              (do   (hasReturn, compoundStmt) <- controlStructureParser <|> subprogramsParser <|> structParser
                    return (hasReturn, compoundStmt)) <|>
                     -- pode ser um compound stmt: estrutura de controle, subpr ou struct
              (do   pure()   
                    -- quando criamos um novo bloco, precisamos
                    -- 1. criar o seu escopo no escopo atual
                    -- 2. executa-lo vendo se tem algum stmt de return em seu corpo
                    -- 3. remover seu escopo do escopo atual

                    s <- getState
                    if (isExecOn s) then do
                      s <- updateAndGetState (addToCurrentScope blockScope) -- entrando num novo escopo
                      (hasReturn, enclosedBlocks) <- enclosedBlocksParser
                      s <- updateAndGetState removeFromCurrentScope -- saindo do escopo criado
                      return (hasReturn, enclosedBlocks)
                    else do
                      (hasReturn, enclosedBlocks) <- enclosedBlocksParser
                      return (hasReturn, enclosedBlocks))
                    -- ou um bloco dentro de chaves


-- <enclosed_blocks> -> LEFT_BRACE <blocks> RIGHT_BRACE
-- bloco entre chaves, eh so um auxiliar mesmo
enclosedBlocksParser :: ParsecT [Token] OurState IO(Bool, [Token])
enclosedBlocksParser = (do  leftBrace <- leftBraceToken
                            (hasReturn, blocks) <- blocksParser -- diz se tem return ou nao nos blocos de dentro
                            rightBrace <- rightBraceToken
                            return (hasReturn, leftBrace:blocks ++ [rightBrace]))


--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------


-- <stmt> -> CONTINUE | BREAK | <return> | <void_command> 
--            | <declrs> | <deref_pointer> <assignments_op> | <stmt_id>
stmtParser :: ParsecT [Token] OurState IO(Bool, [Token])
stmtParser = (do  (flag, token) <- (do  x <- continueToken
                                        return ("continue", x)) <|>
                                   (do  x <- breakToken
                                        return ("break", x))

                  -- parsing de continue e break
                  -- a flag vai identificar qual dos dois foi processado
                  
                  s <- getState

                  -- se estamos de fato executando o codigo, (Exec On)
                  -- assim que chegamos no break ou continue, paramos de executar o loop
                  -- precisamos chegar ate o final de seu corpo, ou seja, vamos ignorar o
                  -- resto dos comandos, por isso o turn off logo abaixo
                  -- alem disso, se o que foi processado foi um BREAK, devemos indicar isso
                  -- alterando a pilha (loopControl)
                  if (isExecOn s) then do 
                    if flag == "break" then do
                      s <- updateAndGetState (setCurrLoopControl BREAK)
                      pure()
                    else do 
                      s <- updateAndGetState (setCurrLoopControl CONTINUE)
                      pure()
                  else do pure()

                  s <- updateAndGetState turnExecOff

                  return (False, [token])) <|> -- retorna falso pq nao sao returns
             (do  x <- voidCommandParser <|> stmtIdParser -- processa comandos void ou (stmtId: assings ou funcall)
                  return (False, x)) <|> -- nao sao returns
             (do  x <- returnParser -- processa um return
                  return (True, x)) <|> -- um return eh um return!!!! retorna TRUE
                  -- abaixo eh a escrita em um ponteiro, para isso, usamos o derefPointerParserWrite
                  -- e o assignmentsOpParserDeref, parsers especificos para essa tarefa
             (do  (derefVal, derefTokens) <- derefPointerParserWrite
                  assignmentsOp <- assignmentsOpParserDeref derefVal
                  return (False, (derefTokens ++ assignmentsOp))) <|> -- nao eh return
             (do  (_, declrs) <- declrsParser "stmt"-- parsing de declaracao
                  return (False, declrs)) -- nao eh reutrn


-- <assignments_op> -> <assign_expr> <remaining_assign>
-- funcao necessaria para assignment em ponteiros dereferenciaos
-- ela ja recebe o (idd e escopo) da variavel alvo
assignmentsOpParserDeref :: VarParam -> ParsecT [Token] OurState IO([Token])
assignmentsOpParserDeref (idd, sp, _) = (do   s <- getState
                                              (exprVal, assignExpr) <- assignExprParser 
                                              -- valor que vai ser atribuido

                                              -- vai fazer um update em (idd,sp) usando exprVal

                                              -- TODO: checar tipos

                                              s <- getState
                                              if isExecOn s then do -- so faz update se exec tiver on
                                                s <- updateAndGetState(memTable UPDATE [] (idd, sp, exprVal))
                                                pure ()
                                              else do pure()

                                              remaining <- remainingAssignsParser -- pode ter mais assigns
                                              return (assignExpr ++ remaining))


-- a.b.c->d->f->g
-- .e.f.t

-- a.b.c == 0 setas -- resolve como sempre resolveu
-- a
-- ->b.c == 1 seta 

-- a.b
-- ->c == 1 seta 

-- a.b->c
-- ->d == 2 setas
-- a.b->c->d
-- ->e.f.g.h == 3 setas

-- aqueal funcao retorna o valor em e

-- (->d)
--   se for seta, a gnt vai 



-- <assignments_op> -> <assign_expr> <remaining_assign>
-- assignment op quando o que eh atribuido nao eh um ponteiro, mas sim um id
-- que pode estar seguido ou nao de modificadores (Acesso a array ou a structs)
assignmentsOpParser :: Token -> [AccessModifier] -> ParsecT [Token] OurState IO([Token])
assignmentsOpParser idd modifiers = (do   (exprVal, assignExpr) <- assignExprParser
                                          -- valor que vai ser atribuido

                                          -- TODO: cchecar tipos

                                          -- 1- separar as listas (antes da ultima seta, depois da ultima seta)
                                          -- 2- aplicar isso na lista antes da ultima seta
                                          --         -- vai ler ate qualquer seta
                                          --         -- vai pegar o ponteiro que ta antes dessa seta
                                          --         -- vai buscar o type que ta salvo nesse ponteiro
                                          --         -- vai repetir passos 1-3`
                                          -- 3- pegar o addr que 2 retorna e fazer um update maroto nele

                                          s <- getState
                                          if isExecOn s then do -- so faz update se exec tiver on
                                            -- splittedModfs <- (return (splitAccessModifiersAtLastArrow modifiers))
                                            (left, right) <- (return (getLastModifiersSepByArrow modifiers))

                                            -- liftIO(print(left))
                                            -- liftIO(print(right))

                                            if((left == []) && (not (headEqualsToP2SAM right))) then do -- a gnt nao tem setinha
                                              -- liftIO(print("AAAAAAAAAAAAA"))
                                              s <- updateAndGetState(memTable UPDATE right (getStringFromId idd, getScope s, exprVal))
                                              pure()
                                            else do
                                              -- liftIO (print(s))
                                              currVal <- (return (getValFromState (getStringFromId idd, getScope s, NULL) s))
                                              currVal <- (return (getValFromValAndModifiers currVal left s))

                                              (targetIdd, targetSp, _) <- (return (getAddrFromPointer currVal))

                                              -- currVal eh um ponteiro cujo valor guaradado no endereco apontando vai ser modificado
                                              s <- updateAndGetState(memTable UPDATE_EXACT (alterModifiersHead right) (targetIdd, targetSp, exprVal))
                                              pure ()
                                            pure ()
                                          else do pure()

                                          remaining <- remainingAssignsParser -- pode ter mais assings
                                          return (assignExpr ++ remaining))



-- <stmt_id> -> ID (<access_modf_op> <assignments_op> | <funcall_op>)
-- eh quando pode ter um assing ou um funcall
stmtIdParser :: ParsecT [Token] OurState IO([Token])
stmtIdParser = (do  idd <- idToken -- o id da variavel que vai ser modificada OU do procedimento

                    -- se o proximo token for um modificador ou um :=, eh pq estamos num assigment
                    -- senao, eh um funcall
                    assignmentOrFuncall <- (do  (maybeAccessModf, accessTokens) <- accessModifierOpParser
                                                assignment <- assignmentsOpParser idd maybeAccessModf
                                                return (accessTokens ++ assignment)) <|>
                                            (do (_, tokens) <- funcallOpParser idd "stmt"
                                                return tokens)
                    return (idd:assignmentOrFuncall))


--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------


-- TODO: documentar
-- <struct> -> STRUCT TYPE_ID LEFT_BRACE { <declrs> }+ RIGHT_BRACE
structParser :: ParsecT [Token] OurState IO(Bool, [Token])
structParser = (do  struct <- structToken
                    idd <- typeIdToken
                    leftBrace <- leftBraceToken
                    s <- getState
                    -- se exec tiver on, vou desliga-lo pra processar as declrs
                    -- e liga-lo novamente. entao, insiro a struct criada na tabela
                    -- de tipos. se exec tiver off (vai estar off? kk) so processo os
                    -- tokens
                    if (isExecOn s) then do
                      s <- updateAndGetState (typesTable INSERT (createStruct (getStringFromId idd) []))
                      s <- updateAndGetState turnExecOff
                      (declrs, declrsTokens) <- multipleDeclrsParser "struct"
                      s <- updateAndGetState turnExecOn
                      rightBrace <- rightBraceToken
                      s <- updateAndGetState (updateType (getStringFromId idd) declrs)
                      return (False, struct:idd:leftBrace:declrsTokens ++ [rightBrace])
                    else do
                      (declrs, declrsTokens) <- multipleDeclrsParser "struct"
                      rightBrace <- rightBraceToken
                      return (False, struct:idd:leftBrace:declrsTokens ++ [rightBrace]))


--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
{-
Os subprogramas serao DECLARADOS com a flag EM_EXEC DESATIVADA
e serao EXECUTADOS com a flag EM_EXEC ATIVADA

Na declaracao, o bloco de tokens do subprograma tambem sera salvo na tabela de funcoes
para futura execucao:
  Quando um subpr for declarado, serao salvos:
    seu protocolo (args)
    seu nome
    seu retorno (se presente o tipo, senao um NULL)
    seus comandos (bloco de execucao)
      esse como uma lista de Tokens
-}

-- <subprograms> -> <func> | <proc>
subprogramsParser :: ParsecT [Token] OurState IO(Bool, [Token])
subprogramsParser = (do   x <- funcParser <|> procParser -- funcao ou proc
                          return (False, x)) -- nao sao returns


-- <func> -> FUNC <type> ID <enclosed_args> <enclosed_blocks>
funcParser :: ParsecT [Token] OurState IO([Token])
funcParser =  (do   func <- funcToken -- o token FUNC
                    (semanType, typee) <- typeParser -- semanType tera o tipo semantico
                    idd <- idToken -- o id da funcao
                    leftParen <- leftParenToken
                    (argsSeman, args) <- argsParser -- ira processar os arugmetnos da chamada
                    rightParen <- rightParenToken

                    subprDeclr <- subprDeclrParser idd argsSeman semanType -- chama o parser geral passando o tipo de retorno
                    return ((func:typee ++ idd:leftParen:args ++ rightParen:subprDeclr)))


-- <proc> -> PROC ID <enclosed_args> <enclosed_blocks>
procParser :: ParsecT [Token] OurState IO([Token])
procParser =  (do   procc <- procToken
                    idd <- idToken -- aqui eh token mesmo
                    leftParen <- leftParenToken
                    (argsSeman, args) <- argsParser -- ira processar os arugmetnos da chamada
                    rightParen <- rightParenToken

                    subprDeclr <- subprDeclrParser idd argsSeman NULL -- chaam o parser geral passando NULL como retorno
                    return (procc:idd:leftParen:args ++ rightParen:subprDeclr))



subprDeclrParser :: Token -> [(String, Type)] -> Type -> ParsecT [Token] OurState IO([Token])
subprDeclrParser idd argsSeman returnSemanType = 
        (do s <- getState
            if (isExecOn s) then do -- o exec devia estar on processando a main 
              s <- updateAndGetState turnExecOff -- desligo temporariamente
              
              -- pego o corpo da funcao, verificnado se as regras de retorno foram respeitads
              (hasReturn, enclosedBlocks) <- enclosedBlocksParser 

              s <- getState
              if (not hasReturn) then undefined -- se nao respeitou as regras, da ruim
              else pure()

              s <- updateAndGetState turnExecOn -- religa o exec

              -- insere a funcao na tabela de subrpgoramas
              s <- updateAndGetState (subProgTable INSERT (getStringFromId idd,
                  returnSemanType, argsSeman, enclosedBlocks))
              return (enclosedBlocks)
            else do -- nao sera chamado ja que nao temos subpr dentro de subpr
              (hasReturn, enclosedBlocks) <- enclosedBlocksParser
              s <- getState
              if (not hasReturn) then undefined
              else pure()
              return (enclosedBlocks))


--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------


-- <control_structures> -> <if> | <while> | <for>
controlStructureParser :: ParsecT [Token] OurState IO(Bool, [Token])
controlStructureParser =  (do   x <- whileParser <|> forParser <|> ifParser
                                return (x))


-- <while> -> WHILE <enclosed_expr> <enclosed_blocks>
whileParser :: ParsecT [Token] OurState IO(Bool, [Token])
whileParser =   (do   while <- whileToken

                      s <- getState

                      if (isExecOn s) then do -- so vou processar se exec tiver on
                        s <- updateAndGetState (turnExecOff) -- boto pra off temporariamente

                        -- a principio, so nos preocupamos com os tokens da definicao,
                        -- ja que esses serao reinseridos no processamento do loop

                        (_, enclosedExpr) <- enclosedExprParser -- a expr de controle

                        (hasReturn, enclosedBlocks) <- enclosedBlocksParser -- o corpo

                        s <- updateAndGetState (turnExecOn) -- ligo novamente
                        
                        s <- updateAndGetState (addToCurrentScope whileScope) -- adiciono while ao escopo
                        executeLoop([], enclosedExpr, [], enclosedBlocks) -- EXECUCAO DO LOOP DE FACTO

                        s <- getState
                        if((isExecOn s)) then do
                          s <- updateAndGetState (removeFromCurrentScope) -- removendo whiel do escopo
                          pure()
                        else do
                          s <- updateAndGetState (turnExecOn) -- ligo novamente
                          s <- updateAndGetState (removeFromCurrentScope) -- removendo whiel do escopo
                          s <- updateAndGetState (turnExecOff) -- ligo novamente
                          pure()

                        return (False, while:enclosedExpr ++ enclosedBlocks) 
                        -- whiles, mesmo se parenco com ifs, podem nunca ser executados,
                        -- logo, nao devem ser condiderados como stmts "com retorno"
                      else do -- aqui vai entrar em defs de subpr, so vai ler os tokens 
                        (_, enclosedExpr) <- enclosedExprParser
                        (hasReturn, enclosedBlocks) <- enclosedBlocksParser
                        return (False, while:enclosedExpr ++ enclosedBlocks)) -- nao eh return


-- att: expr de controle nao pode ser vazia
-- essa funcao so se preocupa em recuperar os tokens,
-- nao em executar o loop. esses tokens serao passados ao um executor
-- esse parser retorna uma tupla com (4 listas de tokens, os tokens totais (que smepre tem q ter))
-- essas 4 listas remetem a: binding inicial, condicao de controle, binding de final de loop e corpo
-- foi preciso fazer essa separacao para implementar o executor sem problemas
forParserAux :: ParsecT [Token] OurState IO(([Token], [Token], [Token], [Token]), [Token])
forParserAux = (do    pure()
                      -- parsing do binding inicial
                      -------------------------------------------------------------
                      maybeVarBinding1 <- (do   (_, declrs) <- declrsParser "stmt"
                                                return (declrs)) <|>
                                                assignmentsParser <|>
                                                (return [])
                      semicolon <- separatorToken

                      -- parsing da expressao de controle
                      -------------------------------------------------------------
                      (_, expr) <- exprParser

                      semicolon <- separatorToken

                      -- bindings do final de loop
                      -------------------------------------------------------------
                      maybeVarBinding2 <- (do   (_, declrs) <- declrsParser "stmt"
                                                return (declrs)) <|>
                                                assignmentsParser <|>
                                                (return [])
                      rightParen <- rightParenToken
                      
                      -- corpo do for
                      -------------------------------------------------------------
                      (_, enclosedBlocks) <- enclosedBlocksParser
                      -------------------------------------------------------------

                      return(((maybeVarBinding1,
                              expr,
                              maybeVarBinding2,
                              enclosedBlocks),
                              maybeVarBinding1 ++ semicolon:expr ++ semicolon:maybeVarBinding2 ++ rightParen:enclosedBlocks)))


-- PARSER PARA EXECUTAR O LOOP: usado tanto para for como while
-- funciona resinserindo os tokens processados (simulando o loop)
-- SO EXECUTA SE EXEC TIVER ON
executeLoop :: ([Token], [Token], [Token], [Token]) -> ParsecT [Token] OurState IO()
executeLoop (initBinding, condExpr, loopBinding, body) = -- binding inicial, cond de controle, binding de final de loop e corpo
  (do   s <- getState
        s <- updateAndGetState (addLoopControl) 
        -- adicionando o controle de loop (pilha de booleanos indicando se o loop ainda eh valido (!= break))
        
        oldTokens <- getInput
        setInput([])
        -- simulando reinsercao dos tokens de binding inicial
        if(initBinding /= []) then do
          setInput (initBinding)

          -- parsing do binding inicial
          _  <- (do   (_, declrs) <- declrsParser "stmt"
                      return (declrs)) <|>
                assignmentsParser <|>
                (return []) -- no caso de while, esse sempre sera o caso (binding inicial inexistente)
          pure()
        else pure ()
        loopRecursiveExecution (condExpr, loopBinding, body) -- execucao do loop de facto
        s <- updateAndGetState removeLoopControl -- removendo o controle de loop da pilha
        setInput(oldTokens)
        return ())


-- funcao para execucao de loop de facto
-- vai ficar testando a condicao e exeuctando o corpo (+ binding de final de loop)
-- SO EXECUTA SE EXEC TIVER ON
-- PODE SAIR COM EXEC OFF, EM CASO DE RETURN
loopRecursiveExecution :: ([Token], [Token], [Token]) -> ParsecT [Token] OurState IO()
loopRecursiveExecution (condExpr, loopBinding, body) =
    (do   pure()
    
          -- simulando entrada de condicao
          remainingTokens <- getInput
          setInput (condExpr ++ remainingTokens)
          -- liftIO(print(condExpr ++ remainingTokens) )

          -- parsing de condicao a fim de checar valor
          (condValue, _) <- exprParser


          if(not (assertType condValue (BoolType False))) then 
            undefined
          else pure()
          -- TODO: compatibilidade
          -- se a expr foi avaliada como true, queremos executar o loop
          if ((getBoolValue condValue) == True) then do

            -- simulando entrada de corpo de loop + binding de final de loop
            remainingTokens <- getInput 
            setInput (body ++ loopBinding ++ remainingTokens)

            -- adicionado o "loop" ao esocpo atual 
            s <- updateAndGetState (addToCurrentScope loopScope)

            s <- updateAndGetState (setCurrLoopControl OK)
            _ <- enclosedBlocksParser -- executando corpo do loop
            
            -- atualizando a var de estado (pode ter ficado obsoleta apos a exeucuao do corpo)
            s <- getState 

            if (isExecOn s) then do
              -- removendo "loop" do escopo atual
              s <- updateAndGetState removeFromCurrentScope
              pure()
            else do
              -- removendo "loop" do escopo atual
              s <- updateAndGetState turnExecOn
              s <- updateAndGetState removeFromCurrentScope
              s <- updateAndGetState turnExecOff
              pure()

            -- se o exec ficou off (rolou um continue ou um break)
            if(not (isExecOn s)) then do
              -- se o loop control ainda ta True, eh pq foi um continue
              -- quero voltar a executar o loop, torno o exec novamente On
              if (getCurrLoopControl s == CONTINUE) then do -- topo da pilha nao foi pra off, loop ainda valido
                s <- updateAndGetState turnExecOn
                pure ()
              else pure ()
            else pure ()

             -- exec fica on so se nao rolar break (rolou continue ou nenhum dos dois)

            if(loopBinding /= []) then do
              -- parsing do final loop binding
              _ <- (do  (_, declrs) <- declrsParser "stmt"
                        return (declrs)) <|>
                        assignmentsParser <|>
                        (return [])
              pure()
            else pure()
            s <- getState -- pegnado estado atualziado

            -- se o exec ta on, eh pq nao rolou um break
            if(isExecOn s) then -- logo, executo o loop novamente
              loopRecursiveExecution (condExpr, loopBinding, body) -- chama recursivamente para o proximo loop
            else if((getCurrLoopControl s) == BREAK) then do
                s <- updateAndGetState turnExecOn -- ligo o exec dnv, o loop acabou !!!
                pure()
            else do pure () -- EH PQ EH UM RETURN, NAO VOLTA O EXEC PRA ON!!
          else pure()
          return ())



-- <for> -> FOR LEFT_PAREN [ ( <declr> | <assignment> ) ] SEMICOLON [ <expr> ] SEMICOLON [ ( <declr> | <assignment> ) ] RIGHT_PAREN <enclosed_blocks>
forParser :: ParsecT [Token] OurState IO(Bool, [Token])
forParser =   (do   for <- forToken
                    leftParen <- leftParenToken

                    s <- getState

                    if isExecOn s then do
                      s <- updateAndGetState (turnExecOff) -- bota exec off temporaraimente

                      -- recuepra os tokens
                      ((initBinding, condExpr, loopBinding, body), tokens) <- forParserAux

                      s <- updateAndGetState (turnExecOn) -- volta exec pra on

                      -- adiciona ao escopo, o loop vai ser iniciado
                      s <- updateAndGetState (addToCurrentScope forScope)

                      -- EXECUCAO DO LOOP
                      executeLoop (initBinding, condExpr, loopBinding, body)

                      -- remove o for do escopo
                      s <- updateAndGetState (removeFromCurrentScope)
                      
                      return (False, for:leftParen:tokens) -- for nao eh return
                    else do
                      (_, tokens) <- forParserAux
                      return (False, for:leftParen:tokens))
                    -- (1) removendo FOR do escopo atual


-- <if> -> IF <enclosed_expr> THEN <enclosed_blocks> [ ELSE ( <if> | <enclosed_blocks> ) ]
ifParser :: ParsecT [Token] OurState IO(Bool, [Token])
ifParser =  (do   iff <- ifToken
                  (exprValue, enclosedExpr) <- enclosedExprParser -- recuperando o valor da condicao
                  s <- getState -- recupera o estado atual



                 
                  if (not (isExecOn s)) then do
                    -- se o exec tiver OFF
                      -- so vai processar os tokens e salvar (do if e do else)
                      -- tambem olha as regras do return (if E else precisam ter return)
                      -- nao havend else, nao ha return no else...
                    (hasReturn, enclosedBlocks) <- enclosedBlocksParser
                    (hasReturnElse, maybeElse) <- maybeElseParser
                    return ((hasReturn && hasReturnElse), iff:enclosedExpr ++ enclosedBlocks ++ maybeElse)
                  else do
                    if(not (assertType (BoolType False) exprValue)) then 
                      undefined
                    else pure()
                    -- se o exec tiver ON
                      -- se a condicao do IF for verdadeira
                        -- execute o corpo do IF
                        -- processe o else com exec OFF
                      -- se a condicao do IF for falsa
                        -- processe o IF com exec OFF
                        -- execute o corpo do ELSE

                    if getBoolValue exprValue then do -- SE A CONDICAO FOR VERDADERIA, ENTRA NO IFF
                      -- adiciona o if ao escopo
                      s <- updateAndGetState (addToCurrentScope ifScope)

                      -- execucao do corpo do if
                      (hasReturn, enclosedBlocks) <- enclosedBlocksParser

                      s <- getState -- recupera estado updt

                      s <- getState
                      if((isExecOn s)) then do
                        s <- updateAndGetState (removeFromCurrentScope) -- removendo whiel do escopo
                        pure()
                      else do
                        s <- updateAndGetState (turnExecOn) -- ligo novamente
                        s <- updateAndGetState (removeFromCurrentScope) -- removendo whiel do escopo
                        s <- updateAndGetState (turnExecOff) -- ligo novamente
                        pure()

                      -- se o exec foi pra OFF durante execucao do corpo do IF
                      if(not(isExecOn s)) then do -- exec ficou falso durante o processametno do if
                        -- processa o else com exec off (ia ser off anyway)
                        (hasReturnElse, maybeElse) <- maybeElseParser
                        return ((hasReturn && hasReturnElse), iff:enclosedExpr ++ enclosedBlocks ++ maybeElse)
                      else do
                        s <- updateAndGetState turnExecOff -- bota off temporariamente pra processar o else
                        (hasReturnElse, maybeElse) <- maybeElseParser -- processa o corpo do MAYBE else
                        s <- updateAndGetState turnExecOn -- joga pra on dnv
                        return ((hasReturn && hasReturnElse), iff:enclosedExpr ++ enclosedBlocks ++ maybeElse)
                    else do -- NAO PASSA NA CONDICAO DO IF
                      s <- updateAndGetState turnExecOff -- bota off temporariamente pra processar o if
                      (hasReturn, enclosedBlocks) <- enclosedBlocksParser -- processa o corpo do if
                      s <- updateAndGetState turnExecOn -- joga on de novo 
                      (hasReturnElse, maybeElse) <- maybeElseParser -- processa o maybe else com exec on
                      -- pode ser que o else jogue o exec pra off. nao precisa tratar isso aqui, 
                      -- ja que continuaria off
                      return ((hasReturn && hasReturnElse), iff:enclosedExpr ++ enclosedBlocks ++ maybeElse))


maybeElseParser :: ParsecT [Token] OurState IO(Bool, [Token])
maybeElseParser = (do   elsee <- elseToken
                        s <- getState
                        if(isExecOn s) then do
                          updateAndGetState (addToCurrentScope elseScope) -- adicona o escopo do else
                          (hasReturn, ifOrEnclosed) <- ifParser <|> enclosedBlocksParser
                          s <- getState -- pegando estado atualizado
                          if((isExecOn s)) then do
                            s <- updateAndGetState (removeFromCurrentScope) -- removendo whiel do escopo
                            pure()
                          else do
                            s <- updateAndGetState (turnExecOn) -- ligo novamente
                            s <- updateAndGetState (removeFromCurrentScope) -- removendo whiel do escopo
                            s <- updateAndGetState (turnExecOff) -- ligo novamente
                            pure()
                          return (hasReturn, elsee:ifOrEnclosed)
                        else do
                          (hasReturn, ifOrEnclosed) <- ifParser <|> enclosedBlocksParser
                          return (hasReturn, elsee:ifOrEnclosed)) <|> -- retorna se tem return ou nao no seu corpo
                  (return (False, [])) -- retorna false pq else vazio nao tem return


----------------------------------------------------------------------------------
----------------------------------------------------------------------------------
----------------------------------------------------------------------------------
----------------------------------------------------------------------------------



-- <type> -> POINTER <enclosed_type>
--           | ARRAY LESS_THAN <expr> COMMA <type> GREATER_THAN
--           | INT | BOOL | DOUBLE | STRING
--           | TYPE_ID
-- parser recursivo para processo de tipo (ex: Int x, processa Int)
-- retorna tambem o tipo semantico (alem dos tokens), que eh a representao interna da lingaugem de tipos
typeParser :: ParsecT [Token] OurState IO (Type, [Token])
typeParser = (do  pure()
                  -- um token de tipo primitivo
                  simpleTypeToken <- intToken <|> boolToken <|> doubleToken <|> stringToken 
                  -- retorna um tipo primitivo na semantica e o token como lista
                  return (createSimpleType simpleTypeToken, [simpleTypeToken])) <|>
             (do  idd <- typeIdToken -- id de tipo definido pelo usuario
                  s <- getState -- da um get tate pra buscar esse tipo na tabela de tipos
                  return (getTypeFromState (getStringFromId idd) s, [idd])) <|>
             (do  array <- arrayToken -- processamento de array
                  lessThan <- lessThanToken

                  (arraySizeVal, size) <- literalParser -- tamanho do array TODO: compatibilidade
                  comma <- commaToken
                  (semanType, typeToken) <- typeParser -- chamada recursiva pra processar o tipo do array
                  greaterThan <- greaterThanToken

                  s <- getState
                  if(not (assertType arraySizeVal (IntType 0))) then do
                    undefined
                  else pure()
                  arraySize <- (return (getIntValue arraySizeVal))
                  -- criacao de um array usando seu tamanho e o tipo processado
                  return (createArray arraySize semanType, (array:lessThan:size ++ comma:typeToken ++ [greaterThan]))) <|>
             (do  x <- pointerToken -- processamento deu um ponteiro
                  lessThan <- lessThanToken
                  (semanType, typeToken) <- typeParser -- chamada recursiva para procesar tipo para o qual o ponteiro aponta
                  greaterThan <- greaterThanToken
                  -- criacao de um ponteiro usando o tipo para o qual ele aponta
                  return (createPointer semanType, x:lessThan:typeToken ++ [greaterThan]))


-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

-- <id> -> ID [<index>]
idParser :: ParsecT [Token] OurState IO([AccessModifier], [Token])
idParser = (do  idd <- idToken -- nome da variavel (so um palavra pro id)
                -- sequencia de modficadores de acesso (struct ou array) (pode ser vazio)
                
                (maybeAccessModf, accessTokens) <- accessModifierOpParser
                return (maybeAccessModf, idd:accessTokens))


-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------



-- <enclosed_expr> -> LEFT_PAREN <expr> RIGHT_PAREN
-- expr entre parenteses
enclosedExprParser :: ParsecT [Token] OurState IO (Type, [Token])
enclosedExprParser = (do  leftParen <- leftParenToken
                          (val, expr) <- exprParser
                          rightParen <- rightParenToken
                          return (val, leftParen:expr ++ [rightParen])) -- retorna o valor processado


-- essa funcao sera para processar operacoes binarias com associatividade a ESQUERDA
-- vai passar um parser de sub expressao, o parser da operacao em questao e um valor (Type) corrente representando 
-- um acumulado (logica de associativade a esquerda) 
-- por ser um remaning, note que a operacao ja foi iniciada (primeiro operando ja foi definido)
remainingExprParser :: (PParser, TokenParser, (Type, [Token])) -> ParsecT [Token] OurState IO (Type, [Token])
remainingExprParser (ruleParser, opParser, x) = (do   op <- opParser -- parsing operacao em questao
                                                      y <- ruleParser -- retorna lista de tokens e o da subexpressao

                                                      s <- getState -- peganod valor atualizado

                                                      -- result eh o resultado do processamento dos operadores a dir
                                                      -- porem, como a associativiade eh a esquerda, a operacao ja eh efetuada
                                                      -- antes da chamada dos operadores a direita (eval eh chamada antes de remainingExprParser)
                                                      -- eval so executa se exec tiver on
                                                      result <- remainingExprParser (ruleParser, opParser, (eval x op y (isExecOn s)))
                                                      return (result)) <|> (return x)


-- essa funcao sera para processar operacoes binarias com associatividade a DIREITA
-- vai passar um parser de sub expressao, o parser da operacao em questao e um valor (Type) corrente representando 
-- o valor a ser operado com o resultado das operacoes a direita
-- por ser um remaning, note que a operacao ja foi iniciada (primeiro operando ja foi definido)
remainingExprParserRight :: (PParser, TokenParser, (Type, [Token])) -> ParsecT [Token] OurState IO (Type, [Token])
remainingExprParserRight (ruleParser, opParser, x) = (do  op <- opParser -- parsing operacao em questao
                                                          y <- ruleParser -- retorna lista de tokens e o da subexpressao
                                                          
                                                          -- so quero somar o oeprando atual com o da direita
                                                          -- depois que o da direita for operado com aqueles
                                                          -- a direita dele

                                                          -- recupera o resultado da operacao aplicada aos operandos a direita do atual
                                                          result <- remainingExprParserRight (ruleParser, opParser, y)
                                                          
                                                          s <- getState

                                                          -- executo a operacao com o operando atual SE O EXEC TIVER ON
                                                          return (eval x op y (isExecOn s))) <|> (return x)


-- SEGUE LISTA DE PARSERS DE EXPRESSOES DE ACORDO COM SUAS PRECEDENCIAS
-- OR
-- AND
-- EQUALS, DIFF
-- GREATER, LESS, GEQ, LEQ
-- PLUS, MINUS
-- TIMES, DIV
-- MINUS, NEG (UNARY OP)
-- EXPONENTIACAO
-- EXPRS EM (), LITERAIS, IDS, ...

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


-- <expr_1> -> <value> [ EXP <expr_2>] -- expr1 chama expr2 pra tratar operadores unarios nos expoentes
expr1Parser :: ParsecT [Token] OurState IO (Type, [Token])
expr1Parser = (do   value <- valueParser
                    result <- remainingExprParserRight (expr2Parser, expoToken, value)
                    return (result))


-- parser de valores pontuais 
-- <value> ->  <deref_pointer> | <literal> | <command_with_ret> | <value_id> | <enclosed_expr>
valueParser :: ParsecT [Token] OurState IO (Type, [Token])
valueParser = (do   literal <- literalParser
                    return literal) <|>
              (do   valueId <- valueIdParser
                    return valueId) <|>
              (do   encExpr <- enclosedExprParser
                    return encExpr) <|>
              (do   command <- commandWithRetParser
                    return command) <|>
              (do   derefPointer <- derefPointerParserRead
                    return derefPointer) <|>
              (do   nullValue <- nullToken
                    return (((PointerType (NULL, ("", ""))),[nullValue])))


-- <literal> -> INT_LIT | BOOL_LIT | DOUBLE_LIT | STRING_LIT
literalParser :: ParsecT [Token] OurState IO(Type, [Token])
literalParser = (do   x <- intLitToken <|> boolLitToken <|> doubleLitToken <|> stringLitToken
                      return (getLiteralType x, [x]))


--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------

-- - <value_id_op> -> [(<index_op> | <funcall>)]
-- parser para processar ID + (funcall OU modifiers)
valueIdOpParser :: Token -> ParsecT [Token] OurState IO(Type, [Token])
valueIdOpParser idd = (do   (returnedVal, tokens) <- funcallOpParser idd "expr" -- chamada de funcao
                            return (returnedVal, tokens)) <|>
                      (do   (modifiers, tokens) <- accessModifierOpParser -- ESSE CARA PODE SER VAZIO!
                            s <- getState
                            -- id seguido de modificadores de acesso
                            if(isExecOn s) then do
                              -- recupera o valor da variavel apos aplicar os modificadores de acesso em idd
                              idType <- (return ((getValFromState (getStringFromId idd, getScope s, NULL) s)))
                              -- liftIO(print(idType))
                              val <- (return (getValFromValAndModifiers idType modifiers s))
                              return ((val, tokens))
                            else do return (NULL, tokens))


-- <value_id> -> ID <value_id_op>
valueIdParser :: ParsecT [Token] OurState IO(Type, [Token])
valueIdParser = (do   idd <- idToken -- idd
                      -- chama o modifers ou funcall passando o idd como param
                      (val, tokens) <- (valueIdOpParser idd) 
                      return (val, idd:tokens))


-- <access_modf_op> -> (LEFT_BRACKET <expr> RIGHT_BRACKET | DOT id) <access_modf_op> | NULL
-- access modifier de struct ou de array
accessModifierOpParser :: ParsecT [Token] OurState IO([AccessModifier], [Token])
accessModifierOpParser = (do  leftBracket <- leftBracketToken -- access de array
                              (valExpr, tokensExpr) <- exprParser -- o index do array TODO: compatibilidade
                              rightBracket <- rightBracketToken
                              (modifiers, tokensRemaining) <- accessModifierOpParser -- chamada recursiva

                              -- cria um access modificer pro array
                              return (((ArrayAM (getIntValue valExpr)):modifiers), leftBracket:tokensExpr ++ rightBracket:tokensRemaining)) <|>
                         (do  dot <- dotToken
                              idd <- idToken -- campo do struct que sera  acessado
                              (modifiers, tokensRemaining) <- accessModifierOpParser -- chamada recursiva

                              -- cria um access modifier pro struct
                              return (((StructAM (getStringFromId idd)):modifiers), dot:[idd])) <|>
                        (do   arrow <- arrowToken
                              idd <- idToken -- campo do struct que sera  acessado
                              (modifiers, tokensRemaining) <- accessModifierOpParser -- chamada recursiva

                              -- cria um access modifier pro struct
                              return (((P2SAM (getStringFromId idd)):modifiers), arrow:[idd])) <|>
                         (return ([],[]))


--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------


-- <deref_pointer> -> STAR <id>
-- deref de pointer para leitura de valor
-- retorna um Type: valor lido
derefPointerParserRead :: ParsecT [Token] OurState IO(Type, [Token])
derefPointerParserRead = (do  stars <- many1 starToken -- lendo PELO MENOS UMA estrela pra acesso de pointer
                              (modifiers, (idd:idTokens)) <- idParser -- recupera o idd e os modifiers semanticos
                              
                              s <- getState
                              if (isExecOn s) then do -- se o exec tiver on
                                -- recupera o valor representado por idParser (idd + modifiers)
                                -- esse valor DEVE ser um ponteiro
                                val <- (return (getValFromValAndModifiers (getValFromState (getStringFromId idd,
                                              getScope s, NULL) s) modifiers s))

                                -- recupera o valor target do ponteiro 
                                return (getPointerValueFromState val (length stars) s, stars ++ (idd:idTokens))
                              else do -- se exex tiver off, nao busca nada, so retorna NULLL + tokens
                                return (NULL, stars ++ (idd:idTokens)))


-- <deref_pointer> -> STAR <id>
-- deref de pointer para escrita de valor
-- retorna um VarParam: id e escopo (endereco) da varaivel que sera modificada
derefPointerParserWrite :: ParsecT [Token] OurState IO(VarParam, [Token])
derefPointerParserWrite = (do   stars <- many1 starToken -- lendo PELO MENOS UMA estrela pra acesso de pointer
                                (modifiers, (idd:idTokens)) <- idParser -- recupera o idd e os modifiers semanticos
                                s <- getState

                                -- liftIO (print ("MODIFIEERSSS"))
                                -- liftIO(print modifiers)

                                if (isExecOn s) then do -- se o exec tiver on
                                  -- recupera o valor representado por idParser (idd + modifiers)
                                  -- esse valor DEVE ser um ponteiro
                                  val <- (return (getValFromValAndModifiers (getValFromState (getStringFromId idd,
                                              getScope s, NULL) s) modifiers s))

                                  -- recupera o valor target do penultimo ponteiro usnado as estrelas
                                  -- o valor do penultimo ponteiro contem o endereco da varaivel que sera modificada
                                  pointerToTarget <- (return (getPointerValueFromState val ((length stars)-1) s))

                                  -- retorna o endereco da varaivel a ser modifciada
                                  return ((getAddrFromPointer pointerToTarget), stars ++ (idd:idTokens))
                                else do -- se exec tiver off, retorna um placeholder
                                  return (("", "", NULL), stars ++ (idd:idTokens)))


--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------


-- vai processar enclosed blocks
-- so sera chamado com EXEC ON
processSubProgParser :: ParsecT [Token] OurState IO (Type)
processSubProgParser = (do  _ <- enclosedBlocksParser -- processa o subprograma
                            s <- getState

                            -- exec pode ir pra off por conta do return
                            -- mando pra on ao final de sua execucao
                            s <- updateAndGetState turnExecOn 

                            -- recuperando valor de var temporaria referetne ao return
                            -- se for proc, essa var eh NULL
                            -- se for func, (deve ser) o tipo do retorno
                            x <- (return (getValFromReturn ((getStringFromSubprCounter s), returnSpecialScope, NULL) s))

                            -- removendo var temporaria referente ao return
                            s <- updateAndGetState (memTable REMOVE [] ((getStringFromSubprCounter s), returnSpecialScope, NULL))
                            return x)


-- <funcall_op> -> LEFT_PARENT <funcall_args> RIGHT_PARENT
-- parser de chamada de subprograma
funcallOpParser :: Token -> String -> ParsecT [Token] OurState IO(Type, [Token])
funcallOpParser idd  parent = (do   leftParen <- leftParenToken

                                    -- parser de argumentos REAIS (argumentos da chamada de subpr)
                                    (valArgs, tokenArgs) <- (do   (valExpr, tokensExpr) <- exprParser
                                                                  (tailVals, tailTokens) <- funcallArgsParser
                                                                  return ((valExpr:tailVals), tokensExpr ++ tailTokens)) <|>
                                                            (return ([], []))
                                    rightParen <- rightParenToken


                                    s <- getState
                                    if (isExecOn s) then do -- so realizo a chamada com o exec ON

                                      -- buscando na lista de subprogramas o registro do subprograma invocado
                                      (retType, argsList, body) <- (return (searchForSubprogFromState (getStringFromId idd) s))

                                      -- se for um procedimento num expr ou uma funcao num stmt, da ruim
                                      if ((parent == "expr") && (retType == NULL)) then do
                                        undefined
                                      else if ((parent == "stmt") && (retType /= NULL)) then do
                                        undefined
                                      else do pure ()

                                      -- (1) adicionando uma nova instancia de escopo ativo na lista de (escopos ativos)
                                      s <- updateAndGetState (addToScopeList)

                                      -- (2) adicionando no escopo atual o id do supprogramda
                                      s <- updateAndGetState (addToCurrentScope (getStringFromId idd))

                                      -- (3) incrementando o contador de subprogramas
                                      s <- updateAndGetState incrActiveSubprCounter

                                      -- liftIO(print("Aaaaaaaaaa"))
                                      -- liftIO(print(argsList))
                                      -- liftIO(print("bbbbbbbbbbb"))
                                      -- liftIO(print(valArgs))

                                      -- parsing e declaracao dos argumentos do subprog
                                      s <- updateAndGetState (declareArgs (getScope s) argsList valArgs)

                                      -- sobrescrevendo o fluxo de tokens restantes do parser pra processar o subpr
                                      remainingTokens <- getInput
                                      setInput (body ++ remainingTokens)

                                      -- chamada do subpr
                                      returnedVal <- processSubProgParser

                                      -- se o tipo retornado for dieferente do tipo reotrnado
                                      if((getDefaultValue returnedVal) /= (getDefaultValue retType)) then do
                                        undefined -- TODO fail?
                                      else do pure ()

                                      -- (3) decrementando o contador de subpr
                                      s <- updateAndGetState decrActiveSubprCounter

                                      -- (2) removendo o escopo do subpr do escopo corrente
                                      s <- updateAndGetState removeFromCurrentScope

                                      -- (1) removendo o escopo corrente referente a essa chamada da pilha
                                      s <- updateAndGetState (removeFromScopeList)

                                      -- retorna o retorno da chamada da func ou proc + tokens
                                      return (returnedVal, leftParen:tokenArgs ++ [rightParen])
                                    else do return (NULL, leftParen:tokenArgs ++ [rightParen])) -- retorna placehodler + tokens


-- parser dos argumetnos de chamada de funcao
funcallArgsParser :: ParsecT [Token] OurState IO([Type], [Token])
funcallArgsParser = (do   comma <- commaToken
                          (valExpr, tokensExpr) <- exprParser -- expressao remetendo a valor de argumento
                          (tailVals, tailTokens) <- funcallArgsParser -- chamada recursiva
                          return ((valExpr:tailVals), comma:tokensExpr ++ tailTokens)) <|>
                    (return ([], []))


-- <return> -> RETURN [ <expr> ]
returnParser :: ParsecT [Token] OurState IO([Token])
returnParser = (do  ret <- returnToken -- token (RETURN)

                    -- o que vai ser retornado. se for nulo, o tipo (valor) semantico eh NULL
                    (valExpr, tokenExpr) <- exprParser <|> (return (NULL, []))

                    s <- getState
                    if (isExecOn s) then do -- se exec tiver on

                      x <- (return (getStringFromSubprCounter s))
                      
                      if(x == "0") then undefined -- TODO: controle de msg de erro
                      else pure ()

                      -- adicionar o valor a ser retornado na variavel temporaria para essa funcao
                      s <- updateAndGetState (memTable INSERT [] ((getStringFromSubprCounter s), returnSpecialScope, valExpr))
                      

                      -- se estou dentro de um loop, defino o loop control como a flag RETURN
                      s <- updateAndGetState (setCurrLoopControl RETURN)

                      -- desligando o exec para nao permitir mais execucao do codigo no subpr
                      s <- updateAndGetState turnExecOff 

                      return (ret:tokenExpr)
                    else do return (ret:tokenExpr))


-- <args> -> <type> ID { COMMA <type> ID } | LAMBDA
-- parser de argumetos de definicao de funcao // ARGUMENTOS FORMAIS
argsParser :: ParsecT [Token] OurState IO([(String, Type)], [Token])
argsParser = (do  (semanType, tokenType) <- typeParser -- tipo semantico e tokens
                  idd <- idToken -- id da variavel (cabecalho de funcao)

                  -- processamento dos argumentos restantes
                  (remainingArgs, remainingArgsTokens) <- remainingArgsParser
                  return (((getStringFromId idd, semanType):remainingArgs, tokenType ++ idd:remainingArgsTokens))) <|>
              (return ([], []))


-- processamento dos argumentos restantes
-- retorna uma lista de (nome, tipo): nome da variavel do argumento e seu tipo
remainingArgsParser :: ParsecT [Token] OurState IO([(String, Type)], [Token])
remainingArgsParser = (do   comma <- commaToken
                            (semanType, tokenType) <- typeParser -- tipo semantico e tokens
                            idd <- idToken -- id da variavel 
                            (tailArgs, tailTokenArgs) <- remainingArgsParser -- chamada recursiva

                            -- merge do argumento atual com os do remaining
                            return (((getStringFromId idd, semanType):tailArgs), comma:tokenType ++ idd:tailTokenArgs)) <|>
                      (return ([], []))

--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------

-- <command_with_ret> -> <alloc> | <addr> | <len> | <cast> | <substr>
-- comandos com return, serao usados em expressoes
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
                  -- chama a funcao de cast da expressao em questtao para o tipo desejado

                  s <- getState -- so faco o cast se exec tiver on
                  if(isExecOn s) then do return (cast exprVal semanType , castT:lp:expr ++ c:t ++ [rp])
                  else return (NULL, castT:lp:expr ++ c:t ++ [rp]))


-- <alloc> -> ALLOC <type>
-- alocacao de um tipo especifico
allocParser :: ParsecT [Token] OurState IO(Type, [Token])
allocParser = (do   alloc <- allocToken
                    lp <- leftParenToken
                    (semanType, tokenType) <- typeParser -- tipo a ser alocado
                    rp <- rightParenToken

                    s <- getState
                    
                    if(isExecOn s) then do -- so realiza alocacao e reucperacao do valor se exec tiver on
                      s <- updateAndGetState (memTable INSERT [] ("", heapScope, semanType))
                      return (getAlloc s semanType, alloc:lp:tokenType ++ [rp])
                    else do
                      return (NULL, alloc:lp:tokenType ++ [rp]))


-- <addr> -> ADDR LEFT_PAREN <id> RIGHT_PAREN
-- recupera addr de uma variavel (sem modificadores)
addrParser :: ParsecT [Token] OurState IO(Type, [Token])
addrParser = ( do addr <- addrToken
                  leftParen <- leftParenToken
                  idd <- idToken -- id da variavel cujo endereco sera buscado
                  rightParen <- rightParenToken

                  s <- getState -- so realizo a busca de o exec tiver on
                  if(isExecOn s) then return (getPointerFromIdFromState (getStringFromId idd) s, addr:leftParen:idd:[rightParen])
                  else return (NULL, addr:leftParen:idd:[rightParen]))



-- <len> -> LEN LEFT_PAREN <id> RIGHT_PAREN
-- recupera o tamanho de uma sring ou de um array
lenParser :: ParsecT [Token] OurState IO(Type, [Token])
lenParser = ( do  len <- lenToken
                  leftParen <- leftParenToken
                  (exprVal, expr) <- exprParser -- TODO: compatibilidade
                  rightParen <- rightParenToken

                  s <- getState
                  if(isExecOn s) then -- so executa a consulta se exec tiver on
                    if(assertType exprVal (StringType "") || assertType exprVal (ArrayType (0,[]))) then
                      return (getLen exprVal, len:leftParen:expr ++ [rightParen])
                    else undefined
                  else  return (NULL, len:leftParen:expr ++ [rightParen]))


--  subst(string, ini, excl),ini:index e inicio, excl: limite da direita exclusivo
-- recupera uma substring no intervalo [init;excl)
-- <substr> -> SUBSTR LEFT_PAREN <expr> COMMA <expr> COMMA <expr> RIGHT_PAREN
subStrParser :: ParsecT [Token] OurState IO(Type, [Token])
subStrParser = (do  substr <- substrToken
                    lp <- leftParenToken
                    (strVal, str) <- exprParser -- string a ser cortada TODO: compatibilidade
                    c1 <- commaToken
                    (leftVal, left) <- exprParser -- limite esquerdo (INCLUSIVO) TODO: compatibilidade
                    c2 <- commaToken
                    (rightVal, right) <- exprParser -- limite direito (EXCL) TODO: compatibilidade
                    rp <- rightParenToken

                    s <- getState
                    if(isExecOn s) then -- so corto a string se exec tiver on
                      if(assertType strVal (StringType "")) then
                        return (getSubstr leftVal rightVal strVal, substr:lp:str ++ c1:left ++ c2:right ++ [rp])
                      else undefined
                    else
                      return (NULL, substr:lp:str ++ c1:left ++ c2:right ++ [rp]))



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
                  
                  -- exprVal tem que ser do tipo PointerType(tipo, id, escopo)  TODO: compatibiliade
                  (exprVal, exprTokens) <- exprParser
                  
                  s <- getState
                  if(isExecOn s) then do -- so executo o free se exec tiver on
                    -- recupera um addr de um ponteiro (idd e escopo)
                    (idd, sp, x) <- (return (getAddrFromPointer exprVal))

                    -- se o escopo for de heap: se o ponteiro guarda um cara na heap
                    if (sp == heapScope) then do 
                      s <- updateAndGetState(memTable REMOVE [] (idd, sp, x)) -- removo ele
                      pure ()
                    else do
                      undefined -- senao da erro
                      pure ()
                    return (free:exprTokens)
                  else return (free:exprTokens))


-- <print> -> PRINT LEFT_PAREN <expr> RIGHT_PAREN
printParser :: ParsecT [Token] OurState IO([Token])
printParser = (do printt <- printToken
                  leftParen <- leftParenToken
                  (val, expr) <- exprParser -- expressao a ser impressa
                  rightParen <- rightParenToken

                  s <- getState
                  if isExecOn s then do -- so executa se exec tiver on 
                      -- if(assertType val (IntType 0) ||
                      --   assertType val (DoubleType 0.0) ||
                      --   assertType val (BoolType False) ||
                      --   assertType val (StringType "")) then do
                      -- liftIO (print s)
                      -- x <- getInput
                      -- liftIO(print(x))
                      liftIO (print val)
                      -- else undefined
                  else pure ()

                  return (printt:leftParen:expr ++ [rightParen]))



-- <read> -> READ LEFT_PAREN <id> RIGHT_PAREN
readParser :: ParsecT [Token] OurState IO([Token])
readParser = (do  readd <- readToken
                  leftParen <- leftParenToken
                  tokens <- readOpParser
                  return (readd:leftParen:tokens))


-- separado em dois caso a leitura seja de um valor de ponteiro
-- esse eh tratado diferentemente de uma leitura de id normal (com ou sem modfs)
readOpParser :: ParsecT [Token] OurState IO([Token])
readOpParser = (do  (modifiers, (idd:tokens)) <- idParser -- ID NORMAL, SEM DEREF
                    rightParen <- rightParenToken
                    s <- getState
                    if(isExecOn s) then do -- so escreve se exec tiver on
                      readVal <- liftIO (getLine) -- leitura de input

                      -- 1. tem que descobrir o tipo da variavel que ta sendo lida (getValFromState)
                      -- 2. tem que converter da string pra esse tipo (convertStringToType) TODO:EMISSAO ERRO DE PARSING PARA TIPO
                      -- 3. tem que fazer o update na memoria -- TODO : COMPATIBILDIADE
                      (left, right) <- (return (getLastModifiersSepByArrow modifiers))


                      if((left == []) && (not (headEqualsToP2SAM right))) then do -- a gnt nao tem setinha
                        idVal <- (return ((getValFromState (getStringFromId idd, getScope s, NULL) s)))
                        oldVal <- (return (getValFromValAndModifiers idVal right s))
                        newVal <- (return (convertStringToType readVal oldVal))
                        s <- updateAndGetState(memTable UPDATE modifiers (getStringFromId idd, getScope s, newVal))
                        pure()
                      else do
                        -- liftIO (print(s))
                        currVal <- (return (getValFromState (getStringFromId idd, getScope s, NULL) s))
                        currVal <- (return (getValFromValAndModifiers currVal left s))

                        (targetIdd, targetSp, _) <- (return (getAddrFromPointer currVal))

                        -- so que nao eh pra checar subpr ativo etc
                        idVal <- (return ((getExactValFromState (targetIdd, targetSp, NULL) s)))
                        oldVal <- (return (getValFromValAndModifiers idVal (alterModifiersHead right) s))
                        newVal <- (return (convertStringToType readVal oldVal))

                        -- currVal eh um ponteiro cujo valor guaradado no endereco apontando vai ser modificado
                        s <- updateAndGetState(memTable UPDATE_EXACT (alterModifiersHead right) (targetIdd, targetSp, newVal))
                        pure ()
                      pure ()
                    else pure ()
                    return ((idd:tokens) ++ [rightParen])) <|>
               (do  ((idd, sp, _), derefTokens) <- derefPointerParserWrite -- DEREF POINTER
                    -- idd e sp formam o endereco da variavel cujo valor sera lido
                    rightParen <- rightParenToken
                    s <- getState
                    if(isExecOn s) then do -- so escrevo se exec tiver on
                      readVal <- liftIO (getLine) -- leitura de input
                     
                      -- 1. tem que descobrir o tipo da variavel que ta sendo lida (getValFromState)
                      -- 2. tem que converter da string pra esse tipo (convertStringToType) TODO:EMISSAO ERRO DE PARSING PARA TIPO
                      -- 3. tem que fazer o update na memoria -- TODO : COMPATIBILDIADE
                      s <- updateAndGetState(memTable UPDATE [] (idd, sp,
                              convertStringToType readVal (getValFromState (idd, sp, NULL) s)))
                      pure ()
                    else pure ()
                    return (derefTokens ++ [rightParen]))

---------------------------------------------------------------------------------
---------------------------------------------------------------------------------
---------------------------------------------------------------------------------

-- <assign_expr> -> ASSIGN <expr>
-- processa um assignOp (:= <expr>)
assignExprParser :: ParsecT [Token] OurState IO(Type, [Token])
assignExprParser = (do  assign <- assignToken
                        (val, expr) <- exprParser
                        return (val, (assign:expr)))


-- <assignments> -> <assignment> <remaining_assign>
-- processa lista de assignments
assignmentsParser :: ParsecT [Token] OurState IO([Token])
assignmentsParser = (do   assignment <- assignmentParser
                          remaining <- remainingAssignsParser
                          return (assignment ++ remaining))



-- <assignment> -> (<id> | <deref_pointer>) <assign_expr>
-- processa uma assign
-- tem q ser separado pra deref pointer tal qual o readParser
assignmentParser :: ParsecT [Token] OurState IO([Token])
assignmentParser = (do  (modifiers, (idd:idTokens)) <- idParser
                        (exprVal, assignExpr) <- assignExprParser -- valor que sera atribuido a var

                        s <- getState
                        if(isExecOn s) then do -- so modifica de exec tiver on
                          (left, right) <- (return (getLastModifiersSepByArrow modifiers))

                          if((left == []) && (not (headEqualsToP2SAM right))) then do -- a gnt nao tem setinha
                            s <- updateAndGetState(memTable UPDATE modifiers (getStringFromId idd, getScope s, exprVal))
                            pure()
                          else do
                            -- liftIO (print(s))
                            currVal <- (return (getValFromState (getStringFromId idd, getScope s, NULL) s))
                            currVal <- (return (getValFromValAndModifiers currVal left s))

                            (targetIdd, targetSp, _) <- (return (getAddrFromPointer currVal))

                            -- currVal eh um ponteiro cujo valor guaradado no endereco apontando vai ser modificado
                            s <- updateAndGetState(memTable UPDATE_EXACT (alterModifiersHead right) (targetIdd, targetSp, exprVal))
                            pure ()
                        else do pure ()

                        return ((idd:idTokens) ++ assignExpr)) <|>
                   (do  pure()
                        -- parser do deref retornando addr da var cujo valor sera modificado
                        ((idd, sp, _), derefTokens) <- derefPointerParserWrite 
                        (exprVal, assignExpr) <- assignExprParser -- valor que sera atribuido a var
                        s <- getState
                        if(isExecOn s) then do -- so modifica de exec tiver on
                          s <- updateAndGetState(memTable UPDATE [] (idd, sp, exprVal))
                          pure ()
                        else pure ()
                        return (derefTokens ++ assignExpr))
                  


-- <remaining_assign> -> { COMMA <assignment> }
-- resto de assings
remainingAssignsParser :: ParsecT [Token] OurState IO([Token])
remainingAssignsParser = (do  remaining <- many (do   comma <- commaToken
                                                      assignment <- assignmentParser
                                                      return (comma:assignment))
                              return (concat(remaining)))


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

maybeAssignExprParser :: String -> ParsecT [Token] OurState IO(Type, [Token])
maybeAssignExprParser parent = (do  if parent == "struct" then do
                                      ass <- assignToken
                                      (val, tokens) <- literalParser
                                      return ((val, ass:tokens))
                                    else do
                                      x <- assignExprParser
                                      return (x)) <|>
                                (return (NULL, []))


-- <declrs> -> <type> <maybe_assigned_id>  { COMMA <maybe_assigned_id>}]
declrsParser :: String -> ParsecT [Token] OurState IO([(String, Type)], [Token])
declrsParser parent = (do   (semanType, typee) <- typeParser
                            idd <- idToken -- aqui eh tokens mesmo

                            (val, maybeAssignExpr) <- maybeAssignExprParser parent

                            s <- getState

                            if(isExecOn(s)) then do
                              if((getDefaultValue(val) /= getDefaultValue(semanType)) &&
                                    (val /= NULL)) then
                                undefined
                              else pure()
                            else pure()                      

                            (tailDeclr, tokens) <- remainingDeclrsParser semanType parent

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
                                s <- updateAndGetState(memTable INSERT [] (getStringFromId idd, getScope s, semanType))
                                return (((getStringFromId idd, semanType):tailDeclr), (typee ++ idd:maybeAssignExpr ++ tokens))
                              else do
                                s <- updateAndGetState(memTable INSERT [] (getStringFromId idd, getScope s, val))
                                return (((getStringFromId idd, val):tailDeclr), (typee ++ idd:maybeAssignExpr ++ tokens))
                            else do
                              if val == NULL then do
                                return (((getStringFromId idd, semanType):tailDeclr), (typee ++ idd:maybeAssignExpr ++ tokens))
                              else do
                                return (((getStringFromId idd, val):tailDeclr), (typee ++ idd:maybeAssignExpr ++ tokens)))


-- <declrs> -> {<declr> SEPARATOR}+
-- parser de uma lista de declaracoes
multipleDeclrsParser :: String -> ParsecT [Token] OurState IO([(String, Type)], [Token])
multipleDeclrsParser parent = (do   (hDeclrs, hTokens) <- declrsParser parent
                                    x <- separatorToken
                                    (tDeclrs, tTokens) <- (multipleDeclrsParser parent) <|> (return ([], []))
                                    return (hDeclrs ++ tDeclrs, hTokens ++ x:tTokens))


-- <maybe_assigned_id> -> ID [<assign_expr>]
remainingDeclrsParser :: Type -> String -> ParsecT [Token] OurState IO([(String, Type)], [Token])
remainingDeclrsParser typee parent = 
          (do comma <- commaToken
              idd <- idToken -- aqui eh token mesmo
              (val, maybeAssignExpr) <- maybeAssignExprParser parent
              -- TODO: struct valor padrao pode dar ruim

              s <- getState
              if(isExecOn(s)) then do
                if(getDefaultValue(val) /= getDefaultValue(typee) &&
                                    val /= NULL) then
                  undefined
                else pure()
              else pure()   

              -- pode ser que VAL == NULL: a variavel nao foi inicializada

              (vals, tokens) <- remainingDeclrsParser typee parent
              s <- getState
              if(isExecOn s) then do -- so insere na memtable quando o exec ta on
                if val == NULL then do 
                  s <- updateAndGetState (memTable INSERT [] (getStringFromId idd, getScope s, typee))
                  return ((getStringFromId idd, typee):vals, comma:idd:maybeAssignExpr ++ tokens)
                else do
                  s <- updateAndGetState (memTable INSERT [] (getStringFromId idd, getScope s, val))
                  return ((getStringFromId idd, val):vals, comma:idd:maybeAssignExpr ++ tokens)
              else
                if val == NULL then do 
                  return ((getStringFromId idd, typee):vals, comma:idd:maybeAssignExpr ++ tokens)
                else do
                  return ((getStringFromId idd, val):vals, comma:idd:maybeAssignExpr ++ tokens)) <|>
          (return ([], []))
