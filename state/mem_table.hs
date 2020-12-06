module MemTable where

import OurState
import OurType

import Text.Parsec

import Lexer

import Control.Monad.IO.Class

import Scope

import UserTypeAuxMemTable



--------------------------------------------------------------------------------
----------------------------------  GETTER   -----------------------------------
--------------------------------------------------------------------------------

-- dada uma var, retorna o tipo/valor no topo da sua pilha
getTypeFromVar :: Var -> Type
getTypeFromVar (idA, spA, (typeHeadA, counterHeadA) : tailA) = typeHeadA


-- funcao pra pegar o alloc mais recente
getAlloc :: OurState -> Type -> Type
getAlloc s t = (PointerType (t, (getDecCounterStr s, heapScope)))

-- funcao aux de getAlloc para pegar valor do counter de heap decrementado
getDecCounterStr :: OurState -> String
getDecCounterStr ((l, counter), subp, t, sp, True, contSubpr, loopStack) = (show (counter-1))
getDecCounterStr _ = undefined


-- GETTERS DE ID E ESCOPO EXATAMENTE IGUAIS
-----------------------------------------------------------------------------------------


-- passando id de variavel pra busca-la na lista aux de variaveis de retorno
getValFromReturn :: VarParam -> OurState -> Type
getValFromReturn (idd, "$$$", _) ((l, _), _, _, _, _, _, _) = 
    getExactTypeFromIdAndScope (idd, "$$$") l
getValFromReturn _ _ = undefined


-- funcao recursiva para pegar valor de deref pointer. aux de getPointerValueFromState
getPointerValueFromList :: Type -> Int -> [Var] -> Type
getPointerValueFromList (PointerType (typee, (idd, sp))) 0 l = (PointerType (typee, (idd, sp)))
getPointerValueFromList (PointerType (typee, (idd, sp))) 1 l = getExactTypeFromIdAndScope (idd, sp) l
getPointerValueFromList (PointerType (typee, (idd, sp))) n l = 
        getPointerValueFromList (getExactTypeFromIdAndScope (idd, sp) l) (n-1) l


-- funcao para pegar valor de deref pointer.
getPointerValueFromState :: Type -> Int -> OurState -> Type
getPointerValueFromState (PointerType x) numQueries ((l, _), _, _, _, _, _, _) =
          getPointerValueFromList (PointerType x) numQueries l
getPointerValueFromState _ _ _ = undefined  


-- buscar somente pro variaveis que batem escopo e id 
getExactTypeFromIdAndScope :: (String, String) -> [Var] -> Type
getExactTypeFromIdAndScope (idA, spA) ((idB, spB, (typeHeadB, _) : tailB):tailVars) =
                  if (idA == idB && spA == spB) then typeHeadB
                  else getExactTypeFromIdAndScope (idA, spA) tailVars
getExactTypeFromIdAndScope _ _ = undefined 


-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------


getPointerFromIdFromState :: String -> OurState -> Type -- (pointerType)
getPointerFromIdFromState idd ((v, _), _, _, _, True, _, _) = getPointerFromIdFromMemTable idd v
getPointerFromIdFromState _ _ = undefined


-- tem que ter controle de escopo, nao ta tendo
getPointerFromIdFromMemTable :: String -> [Var] -> Type -- (pointerType)
getPointerFromIdFromMemTable idA ((idB, spB, (typeHeadB, _) : tailB):tailVars) =
                  if (idA == idB) then (PointerType (typeHeadB, (idB, spB)))
                  else getPointerFromIdFromMemTable idA tailVars
getPointerFromIdFromMemTable _ _ = undefined


-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------


-- dada uma lista de varaveis, pegue aquela VARIAVEL com maior escopo
getVarFromMaxScope :: [Var] -> Var
getVarFromMaxScope [] = undefined
getVarFromMaxScope [x] = x
getVarFromMaxScope ((idA, spA, headA : tailA):(idB, spB, headB : tailB):tailVars) =
              if (length spA > length spB) then do
                getVarFromMaxScope ((idA, spA, headA : tailA):tailVars)
              else do
                getVarFromMaxScope ((idB, spB, headB : tailB):tailVars)


-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------

-- essa funcao vai recuperar um valor do estado atual
-- eh chamado numa acesso a valor de variavel normal,
-- getter pra buscar aquela variavel que vai ser atualizada
-- leva em conta
--- as globais
--- as ativas (na minha chamada de subpr)
---- dessas, aquelas que batem o id e tem maior escopo em comum
---- entao, pegamos aquela q tem maior escopo (a maior dos maiores escopos em comum eh a que deve ser atualizada)
getValFromState :: VarParam -> OurState -> Type
getValFromState varQuery ((l, counter), subp, t, sp, True, contSubpr, loopStack) =
  (getTypeFromVar (getVarFromMaxScope(filterSharingScopeVars varQuery ((filterOnlyActiveVars contSubpr l) ++ (filterOnlyGlobals l)))))
getValFromState _ _ = undefined


-- getter pra buscar aquela variavel que vai ser atualizada
-- leva em conta
--- as globais
--- as ativas (na minha chamada de subpr)
---- dessas, aquelas que batem o id e tem maior escopo em comum
---- entao, pegamos aquela q tem maior escopo (a maior dos maiores escopos em comum eh a que deve ser atualizada)
getVarToUpdate :: Int -> VarParam -> [Var] -> Var
getVarToUpdate counterActive varQuery l =
  getVarFromMaxScope (filterSharingScopeVars varQuery ((filterOnlyActiveVars counterActive l) ++ (filterOnlyGlobals l)))



--------------------------------------------------------------------------------
----------------------------------  FILTERS   -----------------------------------
--------------------------------------------------------------------------------


-- funcao para filtrar somente aquela variaveis que compartilham escopo com a var de query
filterSharingScopeVars :: VarParam -> [Var] -> [Var]
filterSharingScopeVars (idA, spA, _) [] = []
filterSharingScopeVars (idA, spA, tA) ((idB, spB, headB : tailB):tailVars) =
            if ((idA == idB) && (isSuffixOf spB spA)) then do
              ((idB, spB, headB : tailB): (filterSharingScopeVars (idA, spA, tA) tailVars))
            else (filterSharingScopeVars (idA, spA, tA) tailVars)
filterSharingScopeVars _ _ = undefined


-- funcao para filtrar as variaveis globais
filterOnlyGlobals :: [Var] -> [Var]
filterOnlyGlobals [] = []
filterOnlyGlobals ((idA, spA, headA : tailA):tailVars) =
        if (spA == rootScope) then do ((idA, spA, headA : tailA): (filterOnlyGlobals tailVars))
        else do (filterOnlyGlobals tailVars)


-- funcao para filtrar as variaveis da ativacao de subprograma corrente
filterOnlyActiveVars :: Int -> [Var] -> [Var]
filterOnlyActiveVars _ [] = []
filterOnlyActiveVars x ((idA, spA, (typeA, counterA) : tailA):tailVars) =
        if (x == counterA) then do
          ((idA, spA, (typeA, counterA) : tailA): (filterOnlyActiveVars x tailVars))
        else do (filterOnlyActiveVars x tailVars)


--------------------------------------------------------------------------------
----------------------------------  SETTER   -----------------------------------
--------------------------------------------------------------------------------


memTable :: Operation -> [AccessModifier] -> VarParam -> OurState -> OurState
-- update para varaiveis na heap
memTable UPDATE modfs (idd, "$$", newVal) ((l, counter), subp, t, sp, True, contSubpr, loopStack) =
     ((updateMemTableHeap (idd, "$$", newVal) l, counter), subp, t, sp, True, contSubpr, loopStack)
-- update para variaveis no escopo normal
memTable UPDATE modfs x ((l, counter), subp, t, sp, True, contSubpr, loopStack) =
    ((updateMemTable x contSubpr modfs l, counter), subp, t, sp, True, contSubpr, loopStack)
-- insercap de variaveis na heap
memTable INSERT _ (_, "$$", varVal) ((l, counter), subp, t, sp, True, contSubpr, loopStack) =
    ((insertMemTable 0 ((show counter), heapScope, varVal) l, (counter + 1)), subp, t, sp, True, contSubpr, loopStack)
-- insercao de variaveis no escopo normal
memTable INSERT _ x ((l, counter), subp, t, sp, True, contSubpr, loopStack) = 
    ((insertMemTable contSubpr x l, counter), subp, t, sp, True, contSubpr, loopStack)
-- remove de variaveis em geral
memTable REMOVE _ x ((l, counter), subp, t, sp, True, contSubpr, loopStack) =
    ((removeMemTable x l, counter), subp, t, sp, True, contSubpr, loopStack)
memTable _ _ _ _ = undefined


-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------


insertMemTable :: Int -> VarParam -> [Var] -> [Var]
insertMemTable activeSubpr (idA, spA, typeA) []  = [(idA, spA, [(typeA, activeSubpr)])]
insertMemTable activeSubpr (idA, spA, typeA) ((idB, spB, (typeHeadB, counterHeadB) : valListB) : l) =
                    if (idA == idB) && (spA == spB) then do
                      if activeSubpr > counterHeadB then do
                        (idA, spA, (typeA, activeSubpr) : (typeHeadB, counterHeadB) : valListB) : l
                      else undefined
                    else (idB, spB, (typeHeadB, counterHeadB) : valListB) : insertMemTable activeSubpr (idA, spA, typeA) l
insertMemTable activeSubpr (idA, spA, typeA) ((idB, spB, []) : l) = undefined


-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------


removeMemTable :: VarParam -> [Var] -> [Var] -- typeA eh nulo
removeMemTable _ [] = fail "Variable not found"
removeMemTable (idA, spA, typeA) ((idB, spB, (x : valListB)) : l) =
                    if (idA == idB) && (spA == spB) then do
                      if (valListB == []) then l
                      else do (idB, spB, valListB) : l
                    else (idB, spB, (x : valListB)) : removeMemTable (idA, spA, typeA) l
removeMemTable (idA, spA, typeA) ((idB, spB, []) : l) = undefined


-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------

-- 1.filtrar somente as vars globais ou da minha ativacao
-- 2.filtrar somente as vars que dividem escopo comigo
updateMemTable :: VarParam -> Int -> [AccessModifier] -> [Var] -> [Var]
updateMemTable x counterActive modfs l = updateMemTableAux x (getVarToUpdate counterActive x l) modfs l


updateMemTableAux :: VarParam -> Var -> [AccessModifier] -> [Var] -> [Var]
updateMemTableAux _ _ _ [] = undefined
updateMemTableAux (idA, spA, typeA) (idVar, spVar, x) modfs ((idB, spB, (typeHeadB, counterHeadB) : valListB) : l) =
                    if (idVar == idB) && (spVar == spB) then 
                      if (modfs == []) then (idVar, spVar, (typeA, counterHeadB) : valListB) : l
                      else (idVar, spVar, (updateUserType typeHeadB modfs typeA, counterHeadB) : valListB) : l
                    else 
                      (idB, spB, (typeHeadB, counterHeadB) : valListB) : updateMemTableAux (idA, spA, typeA) (idVar, spVar, x) modfs l


-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------


updateMemTableHeap :: VarParam -> [Var] -> [Var]
updateMemTableHeap (idA, spA, typeA) ((idB, spB, (typeHeadB, counterHeadB) : valListB) : l) =
                    if (idA == idB) && (spA == spB) then do
                      ((idA, spA, (typeA, counterHeadB) : valListB) : l)
                    else 
                      (((idB, spB, (typeHeadB, counterHeadB) : valListB)) : updateMemTableHeap (idA, spA, typeA) l)
updateMemTableHeap _ _ = undefined





