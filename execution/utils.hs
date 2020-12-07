module BasicExecUtils where

import OurState
import OurType

import Lexer
import Text.Parsec

-- x.y.z


headEqualsToP2SAM :: [AccessModifier] -> Bool
headEqualsToP2SAM ((P2SAM x):amTail) = True
headEqualsToP2SAM (amHead:amTail) = False
headEqualsToP2SAM [] = False



splitAccessModifiersAtLastArrow :: [AccessModifier] -> [[AccessModifier]]
splitAccessModifiersAtLastArrow [] = [[]]
splitAccessModifiersAtLastArrow ((P2SAM field):tailC) = 
          ([]:[P2SAM field]: splitAccessModifiersAtLastArrow tailC)
splitAccessModifiersAtLastArrow (x:tailC) = 
          (x: (head (splitAccessModifiersAtLastArrow tailC))) : (tail (splitAccessModifiersAtLastArrow tailC))


getLastModifiersSepByArrow :: [AccessModifier] -> ([AccessModifier], [AccessModifier])
getLastModifiersSepByArrow x = 
  (reverse (fst (getLastModifiersSepByArrowAux (reverse x))),
      reverse (snd (getLastModifiersSepByArrowAux (reverse x))))

getLastModifiersSepByArrowAux :: [AccessModifier] -> ([AccessModifier], [AccessModifier])
getLastModifiersSepByArrowAux [] = ([], [])
getLastModifiersSepByArrowAux ((P2SAM field):tail) = (tail, [(P2SAM field)])
getLastModifiersSepByArrowAux (x:tail) = 
        ((fst (getLastModifiersSepByArrowAux tail)), (x:(snd (getLastModifiersSepByArrowAux tail))))


alterModifiersHead :: [AccessModifier] -> [AccessModifier]
alterModifiersHead ((P2SAM x):amTail) = (StructAM x):amTail
alterModifiersHead (amHead:amTail) = amHead:amTail


------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------



assertType :: Type -> Type -> Bool
assertType val typee = (getDefaultValue(val) == getDefaultValue(typee))


-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------


getIntValue :: Type -> Int
getIntValue (IntType x) = x
getIntValue _ = undefined


getBoolValue :: Type -> Bool
getBoolValue (BoolType x) = x
getBoolValue _ = undefined


getStringValue :: Type -> String
getStringValue (StringType s) = s
getStringValue _ = undefined


-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------


getAddrFromPointer :: Type -> VarParam
getAddrFromPointer (PointerType (_, (idd, sp))) = (idd, sp, NULL)
getAddrFromPointer _ = undefined


convertStringToType :: String -> Type -> Type
convertStringToType x (IntType _) = (IntType (read x))
convertStringToType x (DoubleType _) = (DoubleType (read x))
convertStringToType x (BoolType _) =
      if (x == "true") then (BoolType True)
      else do
        if (x == "false") then (BoolType False)
        else undefined
convertStringToType x (StringType _) = (StringType x)
convertStringToType _ _  = undefined


getDefaultValue :: Type -> Type
getDefaultValue (IntType _) = (IntType 0)
getDefaultValue (DoubleType _) = (DoubleType 0.0)
getDefaultValue (BoolType _) = (BoolType False)
getDefaultValue (StringType _) = (StringType "")
getDefaultValue (StructType (idd, _)) = (StructType (idd, []))

getDefaultValue (PointerType (IntType x, _)) = (PointerType (IntType x, ("","")))
getDefaultValue (PointerType (DoubleType x, _)) = (PointerType (DoubleType x, ("","")))
getDefaultValue (PointerType (BoolType x, _)) = (PointerType (BoolType x, ("","")))
getDefaultValue (PointerType (StringType x, _)) = (PointerType (StringType x, ("","")))
getDefaultValue (PointerType ((StructType (idd, _)), _)) = 
        (PointerType ((StructType (idd, [])), ("","")))

getDefaultValue (PointerType (typee, _)) = (PointerType (NULL, ("","")))

getDefaultValue (ArrayType (sizee, (arrayType:_))) = (ArrayType (sizee, [getDefaultValue arrayType]))
getDefaultValue NULL = NULL

-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------


getStringFromId :: Token -> String
getStringFromId (Id _ x) = x
getStringFromId (TypeId _ x) = x
getStringFromId _ = undefined


getLiteralType :: Token -> Type
getLiteralType (IntLit _ x) = (IntType x)
getLiteralType (DoubleLit _ x) = (DoubleType x)
getLiteralType (BoolLit _ x) = (BoolType x)
getLiteralType (StringLit _ x) = (StringType x)
getLiteralType _ = undefined