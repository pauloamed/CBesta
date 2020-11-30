module OurState where

import Lexer
import Text.Parsec

import OurType

--------------------------------------------------------------------------------
----------------------------------  DEFS    ------------------------------------
--------------------------------------------------------------------------------


type Var = (String, String, [Type])
type VarParam = (String, String, Type)
type Func = (String, Type, [Type], [Token])
type Proc = (String, [Type], [Token])

-- Memoria, Funcoes, Procedimentos, Tipos e EM_EXEC
type OurState = (([Var], Int), [Func], [Proc], [Type], String, Bool)



--------------------------------------------------------------------------------
-----------------------------------  EXEC   ------------------------------------
--------------------------------------------------------------------------------


turnExecOn :: OurState -> OurState
turnExecOn (v, f, p, tl, sp, _) = (v, f, p, tl, sp, True)

turnExecOff :: OurState -> OurState
turnExecOff (v, f, p, tl, sp, _) = (v, f, p, tl, sp, False)

toggleExec :: OurState -> OurState
toggleExec (v, f, p, tl, sp, x) = (v, f, p, tl, sp, not x)

isExecOn :: OurState -> Bool
isExecOn (_, _, _, _, _, b) = b


--------------------------------------------------------------------------------
-------------------------------  TABLE_UTILS   ---------------------------------
--------------------------------------------------------------------------------

data Operation =
  INSERT |
  REMOVE |
  UPDATE
  deriving (Enum)
