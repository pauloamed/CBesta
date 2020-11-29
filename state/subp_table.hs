module SubProgTable where

import OurState

funcTable :: Operation -> Func -> OurState -> OurState
funcTable INSERT f (v, fl, p, t, sp, e) = (v, insertFuncTable f fl, p, t, sp, e)
funcTable _ _ _ = undefined

insertFuncTable :: Func -> [Func] -> [Func]
insertFuncTable f []  = [f]
insertFuncTable f funcTable = funcTable ++ [f]


procTable :: Operation -> Proc -> OurState -> OurState
procTable INSERT p (v, f, pl, t, sp, e) = (v, f, insertProcTable p pl, t, sp, e)
procTable _ _ _ = undefined

insertProcTable :: Proc -> [Proc] -> [Proc]
insertProcTable p []  = [p]
insertProcTable p procTable = procTable ++ [p]
