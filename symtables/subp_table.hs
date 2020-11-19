
insertFuncTable :: (String * Type * [Type]) -> [(String * Type * [Type])] -> [((String * Type * [Type]))]
insertFuncTable f []  = [f]
insertFuncTable f funcTable = funcTable ++ [f]


insertProcTable :: (String * [Type]) -> [(String * [Type])] -> [((String * [Type]))]
insertProcTable p []  = [p]
insertProcTable p procTable = procTable ++ [p]
