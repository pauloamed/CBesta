module SubProgTable where

import OurState
import OurType

import Lexer

subProgTable :: Operation -> SubProg -> OurState -> OurState
subProgTable INSERT subp (v, subpl, t, sp, True, contSubpr, loopStack) = (v, insertSubProgTable subp subpl, t, sp, True, contSubpr, loopStack)
subProgTable _ _ _ = undefined


insertSubProgTable :: SubProg -> [SubProg] -> [SubProg]
insertSubProgTable subp []  = [subp]
insertSubProgTable subp subProgTable = subProgTable ++ [subp]


searchForSubprogFromState :: String -> OurState -> SubProgContent
searchForSubprogFromState idd (_, subpl, _, _, True, _, _) = searchForSubprog idd subpl
searchForSubprogFromState idd _ = undefined


searchForSubprog :: String -> [SubProg] -> SubProgContent
searchForSubprog idd [] = undefined
searchForSubprog idd ((subpId, retType, args, body):subpTail) =
        if idd == subpId then (retType, args, body)
        else searchForSubprog idd subpTail


incrActiveSubprCounter :: OurState -> OurState
incrActiveSubprCounter (v, subpl, t, sp, True, contSubpr, loopStack) = (v, subpl, t, sp, True, contSubpr + 1, loopStack)


decrActiveSubprCounter :: OurState -> OurState
decrActiveSubprCounter (v, subpl, t, sp, True, contSubpr, loopStack) = (v, subpl, t, sp, True, contSubpr - 1, loopStack)


getStringFromSubprCounter :: OurState -> String
getStringFromSubprCounter (_, _, _, _, _, contSubpr, _) = (show contSubpr)
