

data Type =
    IntType Int |
    DoubleType Double |
    BoolType Bool |
    StringType String |
    Struct (String * [String * Type]) | -- nome seguido de uma lista de nomes e Types (tipo + valor) |
    Pointer (Type * (String * String)) |
    Array (Int * [Type]) | -- tamanho, lista com variaveis/valores, escopo e nome. como garantir consistencia de tipo?
    Tuple (Int * [Type]) -- lista de vars, escopo e nome
