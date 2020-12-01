module OurState where

import Lexer
import Text.Parsec

import OurType

--------------------------------------------------------------------------------
----------------------------------  DEFS    ------------------------------------
--------------------------------------------------------------------------------


type Var = (String, String, [Type])
type VarParam = (String, String, Type)
type SubProg = (String, Type, [(String, Type)], [Token])

-- Memoria, Funcoes, Procedimentos, Tipos e EM_EXEC
type OurState = (([Var], Int), [SubProg], [Type], String, Bool)



--------------------------------------------------------------------------------
-----------------------------------  EXEC   ------------------------------------
--------------------------------------------------------------------------------


turnExecOn :: OurState -> OurState
turnExecOn (v, subp, tl, sp, _) = (v, subp, tl, sp, True)

turnExecOff :: OurState -> OurState
turnExecOff (v, subp, tl, sp, _) = (v, subp, tl, sp, False)

toggleExec :: OurState -> OurState
toggleExec (v, subp, tl, sp, x) = (v, subp, tl, sp, not x)

isExecOn :: OurState -> Bool
isExecOn (_, _, _, _, b) = b


--------------------------------------------------------------------------------
-------------------------------  TABLE_UTILS   ---------------------------------
--------------------------------------------------------------------------------

data Operation =
  INSERT |
  REMOVE |
  UPDATE
  deriving (Enum)
