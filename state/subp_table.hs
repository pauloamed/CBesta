module SubProgTable where

import OurState
import OurType

import Lexer

subProgTable :: Operation -> SubProg -> OurState -> OurState
subProgTable INSERT subp (v, subpl, t, sp, e) = (v, insertSubProgTable subp subpl, t, sp, e)
subProgTable _ _ _ = undefined


insertSubProgTable :: SubProg -> [SubProg] -> [SubProg]
insertSubProgTable subp []  = [subp]
insertSubProgTable subp subProgTable = subProgTable ++ [subp]


searchForSubprogFromState :: String -> OurState -> (Type, [(String, Type)], [Token])
searchForSubprogFromState idd (_, subpl, _, _, _) = searchForSubprog idd subpl


searchForSubprog :: String -> [SubProg] -> (Type, [(String, Type)], [Token])
searchForSubprog idd [] = undefined
searchForSubprog idd ((subpId, retType, args, body):subpTail) =
        if idd == subpId then (retType, args, body)
        else searchForSubprog idd subpTail
