module SubProgTable where

import OurState

funcTable :: Operation -> Func -> OurState -> OurState
funcTable INSERT f (v, fl, p, t) = (v, insertFuncTable f fl, p, t)
funcTable _ _ _ = undefined

insertFuncTable :: Func -> [Func] -> [Func]
insertFuncTable f []  = [f]
insertFuncTable f funcTable = funcTable ++ [f]


procTable :: Operation -> Proc -> OurState -> OurState
procTable INSERT p (v, f, pl, t) = (v, f, insertProcTable p pl, t)
procTable _ _ _ = undefined

insertProcTable :: Proc -> [Proc] -> [Proc]
insertProcTable p []  = [p]
insertProcTable p procTable = procTable ++ [p]
