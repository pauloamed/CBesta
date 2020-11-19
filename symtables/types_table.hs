insertTypesTable :: Type -> [Type] -> [Type]
insertTypesTable t []  = [t]
insertTypesTable t typesTable = typesTable ++ [t]
