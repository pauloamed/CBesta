{
module Main (main, Token(..), alexScanTokens) where
}


%wrapper "basic"

$digit = 0-9          -- digits
$alpha = [a-zA-Z]     -- alphabetic characters


---------------------- MAIN  -------------------------
tokens :-

    $white+                                 ; -- ignora espaços em branco
    "//".*                                  ; -- ignora comentários
    (int | double | bool | string)          { \s -> Type s}
    return                                  { \s -> Return}
    import                                  { \s -> Import}
    main                                    { \s -> Main}
    func                                    { \s -> Func}
    proc                                    { \s -> Proc}
    $alpha [$alpha $digit \_ \']*           { \s -> Id s}
    "#"                                     { \s -> Hashtag}
    ":"                                     { \s -> Colon}
    ";"                                     { \s -> SemiColon}
    \n+                                     { \s -> NewLine}
    ","                                     { \s -> Comma}
    \.                                      { \s -> Dot}


----------------------- FLOW  ------------------------
    if                                      { \s -> If}
    then                                    { \s -> Then}
    else                                    { \s -> Else}
    while                                   { \s -> While}
    for                                     { \s -> For}
    continue                                { \s -> Continue}
    break                                   { \s -> Break}

---------------------- SCOPES  -------------------------

    \{                                      { \s -> LeftBrace}
    \}                                      { \s -> RightBrace}
    \(                                      { \s -> LeftParen}
    \)                                      { \s -> RightParen}
    \[                                      { \s -> LeftBracket}
    \]                                      { \s -> RightBracket}

--------------------- OPERADORES  --------------------------

    :=                                      { \s -> Assign}
    "%"                                     { \s -> Mod}
    \^                                      { \s -> Expo}
    \+                                      { \s -> Plus}
    \-                                      { \s -> Minus}
    \*                                      { \s -> Times}
    "/"                                     { \s -> Div}
    "<"                                     { \s -> LessThan}
    >                                       { \s -> GreaterThan}
    "<"=                                    { \s -> LessEquals}
    >=                                      { \s -> GreaterEquals}
    ==                                      { \s -> Equals}
    !=                                      { \s -> Difference}
    "<"\_                                   { \s -> LessThan_}
    >\_                                     { \s -> GreaterThan_}
    "<"=\_                                  { \s -> LessEquals_}
    >=\_                                    { \s -> GreaterEquals_}
    ==\_                                    { \s -> Equals_}
    !=\_                                    { \s -> Difference_}
    !                                       { \s -> Negation}
    (&& | and)                              { \s -> And}
    (\|\| | or)                             { \s -> Or}

-------------------- LITERALS --------------------------

    \-?$digit+\.$digit+                  { \s -> Double(read s) }
    \-?$digit+                           { \s -> Int(read s) }
    true                                { \s -> Bool(read s) }
    false                               { \s -> Bool(read s) }
    \"$alpha [$alpha $digit ! \_ \']*\" { \s -> String s}


{
data Token =
-- MAIN  ---------------------------------------------
    Type String |
    Id String |
    Return |
    Import |
    Main |
    Func |
    Proc |
    Hashtag |
    SemiColon |
    Colon |
    NewLine |
    Comma |
    Dot |
-- FLOW  ---------------------------------------------
    If |
    Then |
    Else |
    While |
    For |
    Continue |
    Break |
-- SCOPES  ---------------------------------------------
    LeftBrace |
    RightBrace |
    LeftParen |
    RightParen |
    LeftBracket |
    RightBracket |
-- OPERADORES  -----------------------------------------
    Assign |
    Mod |
    Expo |
    Plus |
    Minus |
    Times |
    Div |
    LessThan |
    GreaterThan |
    LessEquals |
    GreaterEquals |
    Equals |
    Difference |
    LessThan_ |
    GreaterThan_ |
    LessEquals_ |
    GreaterEquals_ |
    Equals_ |
    Difference_ |
    Negation |
    And |
    Or |
-- LITERALS  -------------------------------------------
    Double Double |
    Int Int |
    Bool Bool |
    String String
    deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}
