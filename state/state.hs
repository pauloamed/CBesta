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
type OurState = (([Var], Int), [SubProg], [Type], [String], Bool, Int)

--------------------------------------------------------------------------------
-----------------------------------  EXEC   ------------------------------------
--------------------------------------------------------------------------------


turnExecOn :: OurState -> OurState
turnExecOn (v, subp, tl, sp, _, contSubpr) = (v, subp, tl, sp, True, contSubpr)

turnExecOff :: OurState -> OurState
turnExecOff (v, subp, tl, sp, _, contSubpr) = (v, subp, tl, sp, False, contSubpr)

toggleExec :: OurState -> OurState
toggleExec (v, subp, tl, sp, x, contSubpr) = (v, subp, tl, sp, not x, contSubpr)

isExecOn :: OurState -> Bool
isExecOn (_, _, _, _, b, _) = b


--------------------------------------------------------------------------------
-------------------------------  PARSER   ---------------------------------
--------------------------------------------------------------------------------

updateAndGetState :: (OurState -> OurState) -> ParsecT [Token] OurState IO(OurState)
updateAndGetState f = (do   updateState f
                            s <- getState
                            return s)


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

--------------------------------------------------------------------------------
-------------------------------  TABLE_UTILS   ---------------------------------
--------------------------------------------------------------------------------

data Operation =
  INSERT |
  REMOVE |
  UPDATE
  deriving (Enum)
