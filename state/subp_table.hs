module SubProgTable where

import OurState
import OurType

import Lexer

subProgTable :: Operation -> SubProg -> OurState -> OurState
subProgTable INSERT subp (v, (subpl, activeSubprCounter), t, sp, e) = (v, (insertSubProgTable subp subpl, activeSubprCounter), t, sp, e)
subProgTable _ _ _ = undefined


insertSubProgTable :: SubProg -> [SubProg] -> [SubProg]
insertSubProgTable subp []  = [subp]
insertSubProgTable subp subProgTable = subProgTable ++ [subp]


searchForSubprogFromState :: String -> OurState -> (Type, [(String, Type)], [Token])
searchForSubprogFromState idd (_, (subpl, _), _, _, _) = searchForSubprog idd subpl


searchForSubprog :: String -> [SubProg] -> (Type, [(String, Type)], [Token])
searchForSubprog idd [] = undefined
searchForSubprog idd ((subpId, retType, args, body):subpTail) =
        if idd == subpId then (retType, args, body)
        else searchForSubprog idd subpTail


incrActiveSubprCounter :: OurState -> OurState
incrActiveSubprCounter (v, (subpl, activeSubprCounter), t, sp, e) = (v, (subpl, (activeSubprCounter + 1)), t, sp, e)


decrActiveSubprCounter :: OurState -> OurState
decrActiveSubprCounter (v, (subpl, activeSubprCounter), t, sp, e) = (v, (subpl, (activeSubprCounter - 1)), t, sp, e)


getStringFromSubprCounter :: OurState -> String
getStringFromSubprCounter (_, (_, activeSubprCounter), _, _, _) = (show activeSubprCounter)
