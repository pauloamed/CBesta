module UserTypeAuxMemTable where

import OurState
import OurType

import Lexer
import Text.Parsec


updateUserType :: Type -> [AccessModifier] -> Type -> Type
updateUserType (StructType (idd, fields)) modfs newVal = updateStruct (StructType (idd, fields)) modfs newVal
updateUserType (ArrayType (size, elements)) modfs newVal = updateArray (ArrayType (size, elements)) modfs newVal
updateUserType _ _ _ = undefined

---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------


updateArray :: Type -> [AccessModifier] -> Type -> Type
updateArray (ArrayType (size, elements)) ((ArrayAM index):tailModf) newVal =
    (ArrayType (size, updateArrayElementsList elements ((ArrayAM index):tailModf) newVal))
updateArray _ _ _ = undefined


updateArrayElementsList :: [Type] -> [AccessModifier] -> Type -> [Type]
updateArrayElementsList (typesHead:typesTail) ((ArrayAM index):tailModf) newVal = 
  if (index == 0) then do
    if (tailModf == []) then do (newVal:typesTail)
    else ((updateUserType typesHead tailModf newVal) : typesTail)
  else (typesHead:(updateArrayElementsList (typesTail) ((ArrayAM (index-1)):tailModf) newVal))


---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------


updateStruct :: Type -> [AccessModifier] -> Type -> Type
updateStruct (StructType (idd, fields)) ((StructAM fieldName):tailModf) newVal =
    (StructType (idd, updateStructFieldsList fields ((StructAM fieldName):tailModf) newVal))
updateStruct _ _ _ = undefined


updateStructFieldsList :: [(String, Type)] -> [AccessModifier] -> Type -> [(String, Type)]
updateStructFieldsList [] ((StructAM fieldName):tailModf) newVal = undefined
updateStructFieldsList ((currFieldName, currFieldVal):fieldsTail) ((StructAM fieldName):tailModf) newVal = 
  if (currFieldName == fieldName) then do
    if (tailModf == []) then do ((fieldName, newVal):fieldsTail)
    else ((fieldName, (updateUserType currFieldVal tailModf newVal)) : fieldsTail)
  else ((currFieldName, currFieldVal):(updateStructFieldsList (fieldsTail) ((StructAM fieldName):tailModf) newVal))