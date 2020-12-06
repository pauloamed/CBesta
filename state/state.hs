module OurState where

import Lexer
import Text.Parsec

import OurType

--------------------------------------------------------------------------------
----------------------------------  DEFS    ------------------------------------
--------------------------------------------------------------------------------


-- id, scopo, pilha de valores, ultima execucao
type Var = (String, String, [(Type, Int)])
type VarParam = (String, String, Type)
type SubProg = (String, Type, [(String, Type)], [Token])
type SubProgContent = (Type, [(String, Type)], [Token])

-- Memoria, Funcoes, Procedimentos, Tipos e EM_EXEC
type OurState = (([Var], Int), [SubProg], [Type], [String], Bool, Int, [(LoopControlType, Int)])

--------------------------------------------------------------------------------
-----------------------------------  EXEC   ------------------------------------
--------------------------------------------------------------------------------


turnExecOn :: OurState -> OurState
turnExecOn (v, subp, tl, sp, _, contSubpr, loopStack) = (v, subp, tl, sp, True, contSubpr, loopStack)

turnExecOff :: OurState -> OurState
turnExecOff (v, subp, tl, sp, _, contSubpr, loopStack) = (v, subp, tl, sp, False, contSubpr, loopStack)

toggleExec :: OurState -> OurState
toggleExec (v, subp, tl, sp, x, contSubpr, loopStack) = (v, subp, tl, sp, not x, contSubpr, loopStack)

isExecOn :: OurState -> Bool
isExecOn (_, _, _, _, b, _, _) = b


--------------------------------------------------------------------------------
-------------------------------  PARSER   ---------------------------------
--------------------------------------------------------------------------------

updateAndGetState :: (OurState -> OurState) -> ParsecT [Token] OurState IO(OurState)
updateAndGetState f = (do   updateState f
                            s <- getState
                            return s)



returnSpecialScope :: String
returnSpecialScope = "$$$"


heapScope :: String
heapScope = "$$"

rootScope :: String
rootScope = "$"

blockScope :: String
blockScope = ""

ifScope :: String
ifScope = "if"

elseScope :: String
elseScope = "else"

forScope :: String
forScope = "for"

whileScope :: String
whileScope = "while"

loopScope :: String
loopScope = "loop"

--------------------------------------------------------------------------------
-----------------------------------  LOOP IF------------------------------------
--------------------------------------------------------------------------------

addLoopControl :: OurState -> OurState
addLoopControl (v, subp, tl, sp, e, contSubpr, loopStack) = (v, subp, tl, sp, e, contSubpr, ((OK, contSubpr):loopStack))


removeLoopControl :: OurState -> OurState
removeLoopControl (v, subp, tl, sp, e, contSubpr, ((_, _):loopStack)) = (v, subp, tl, sp, e, contSubpr, loopStack)
removeLoopControl _ = undefined


setCurrLoopControl :: LoopControlType -> OurState -> OurState
setCurrLoopControl lc (v, subp, tl, sp, e, contSubpr, ((oldValue, snapshotContSubr):loopStack)) = 
      if(contSubpr==snapshotContSubr) then do 
        (v, subp, tl, sp, e, contSubpr, ((lc, snapshotContSubr):loopStack))
      else do (v, subp, tl, sp, e, contSubpr, ((oldValue, snapshotContSubr):loopStack))
setCurrLoopControl _ _ = undefined


getCurrLoopControl :: OurState -> LoopControlType
getCurrLoopControl (v, subp, tl, sp, e, contSubpr, ((curr, _):loopStack)) = curr
getCurrLoopControl _ = undefined

--------------------------------------------------------------------------------
-------------------------------  TABLE_UTILS   ---------------------------------
--------------------------------------------------------------------------------

data Operation =
  INSERT |
  REMOVE |
  UPDATE 
  deriving (Enum)
