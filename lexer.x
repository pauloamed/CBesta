{
module Lexer (Token(..), AlexPosn(..), alexScanTokens, getTokens) where

import System.IO
import System.IO.Unsafe
}



%wrapper "posn"

$digit = 0-9          -- digits
$lowerAlpha = [a-z]     -- alphabetic characters
$upperAlpha = [A-Z]
$alpha = [a-zA-Z]

tokens :-

----------------------- FLOW  --------------------------
    if                                      { \p s -> If p }
    then                                    { \p s -> Then p }
    else                                    { \p s -> Else p }
    while                                   { \p s -> While p }
    for                                     { \p s -> For p }
    continue                                { \p s -> Continue p }
    break                                   { \p s -> Break p }

----------------------- COMMANDS  --------------------------
    free                                    { \p s -> Free p }
    print                                   { \p s -> Print p }
    read                                    { \p s -> Read p }
    alloc                                   { \p s -> Alloc p }
    cast                                    { \p s -> Cast p }
    addr                                    { \p s -> Addr p }
    len                                     { \p s -> Len p }
    substr                                  { \p s -> Substr p }

---------------------- TYPES  -------------------------

    Int                                     { \p s -> Int p }
    Double                                  { \p s -> Double p }
    Bool                                    { \p s -> Bool p }
    String                                  { \p s -> String p }
    Pointer                                 { \p s -> Pointer p }
    List                                    { \p s -> List p }
    Array                                   { \p s -> Array p }
    Hashmap                                 { \p s -> Hashmap p }

---------------------- SCOPES  -------------------------

    \(                                      { \p s -> LeftParen p }
    \)                                      { \p s -> RightParen p }
    \[                                      { \p s -> LeftBracket p }
    \]                                      { \p s -> RightBracket p }
    \{                                      { \p s -> LeftBrace p }
    \}                                      { \p s -> RightBrace p }

--------------------- OPERADORES  --------------------------

    :=                                      { \p s -> Assign p }
    "->"                                    { \p s -> Arrow p }
    "%"                                     { \p s -> Mod p }
    \^                                      { \p s -> Expo p }
    \+                                      { \p s -> Plus p }
    \-                                      { \p s -> Minus p }
    \*                                      { \p s -> Star p }
    "/"                                     { \p s -> Div p }
    "<"                                     { \p s -> LessThan p }
    >                                       { \p s -> GreaterThan p }
    "<"=                                    { \p s -> LessEquals p }
    >=                                      { \p s -> GreaterEquals p }
    ==                                      { \p s -> Equals p }
    !=                                      { \p s -> Difference p }
    "<"\_                                   { \p s -> LessThan_ p }
    >\_                                     { \p s -> GreaterThan_ p }
    "<"=\_                                  { \p s -> LessEquals_ p }
    >=\_                                    { \p s -> GreaterEquals_ p }
    ==\_                                    { \p s -> Equals_ p }
    !=\_                                    { \p s -> Difference_ p }
    !                                       { \p s -> Negation p }
    (&& | and)                              { \p s -> And p }
    (\|\| | or)                             { \p s -> Or p }

-------------------- LITERALS --------------------------
    NULL                                    { \p s -> Null p }
    $digit+\.$digit+                        { \p s -> DoubleLit p (read s) }
    $digit+                                 { \p s -> IntLit p (read s) }
    ("true" | "false")                      { \p s -> BoolLit p (boolVal s) }
    \"[^\"]*\"                              { \p s -> StringLit p (extractString s) }

-------------------------- MAIN ------------------------------------
    $white+                                 ;
    "//".*                                  ; -- ignora comentários
    return                                  { \p s -> Return p }
    import                                  { \p s -> Import p }
    struct                                  { \p s -> Struct p }
    func                                    { \p s -> Func p }
    proc                                    { \p s -> Proc p }
    $lowerAlpha [$alpha $digit \_]*         { \p s -> Id p s }
    $upperAlpha [$alpha $digit \_]*         { \p s -> TypeId p s }
    "#"                                     { \p s -> Hashtag p }
    ":"                                     { \p s -> Colon p }
    ";"                                     { \p s -> Separator p }
    ","                                     { \p s -> Comma p }
    \.                                      { \p s -> Dot p }
{
data Token =
-- FLOW  ---------------------------------------------
    If AlexPosn |
    Then AlexPosn |
    Else AlexPosn |
    While AlexPosn |
    For AlexPosn |
    Continue AlexPosn |
    Break AlexPosn |
-- COMMANDS  ---------------------------------------------
    Free AlexPosn |
    Print AlexPosn |
    Read AlexPosn |
    Alloc AlexPosn |
    Addr AlexPosn |
    Cast AlexPosn |
    Len AlexPosn |
    Substr AlexPosn |
-- TYPES  ---------------------------------------------
    Int AlexPosn |
    Double AlexPosn |
    Bool AlexPosn |
    String AlexPosn |
    Pointer AlexPosn |
    List AlexPosn |
    Array AlexPosn |
    Hashmap AlexPosn |
    Tuple AlexPosn |
-- SCOPES  ---------------------------------------------
    LeftBrace AlexPosn |
    RightBrace AlexPosn |
    LeftParen AlexPosn |
    RightParen AlexPosn |
    LeftBracket AlexPosn |
    RightBracket AlexPosn |
-- OPERADORES  -----------------------------------------
    Arrow AlexPosn |
    Assign AlexPosn |
    Mod  AlexPosn |
    Expo AlexPosn |
    Plus AlexPosn |
    Minus AlexPosn |
    Star AlexPosn |
    Div AlexPosn |
    LessThan AlexPosn |
    GreaterThan AlexPosn |
    LessEquals AlexPosn |
    GreaterEquals AlexPosn |
    Equals AlexPosn |
    Difference AlexPosn |
    LessThan_ AlexPosn |
    GreaterThan_ AlexPosn |
    LessEquals_ AlexPosn |
    GreaterEquals_ AlexPosn |
    Equals_ AlexPosn |
    Difference_ AlexPosn |
    Negation AlexPosn |
    And AlexPosn |
    Or AlexPosn |
-- LITERALS  -------------------------------------------
    DoubleLit AlexPosn Double |
    Null AlexPosn |
    IntLit AlexPosn Int |
    BoolLit AlexPosn Bool |
    StringLit AlexPosn String |
-- MAIN  ---------------------------------------------
    Struct AlexPosn |
    Id AlexPosn String |
    TypeId AlexPosn String |
    Return AlexPosn |
    Import AlexPosn |
    Func AlexPosn |
    Proc AlexPosn |
    Hashtag AlexPosn |
    Separator AlexPosn |
    Colon AlexPosn |
    Comma AlexPosn |
    Dot AlexPosn
    deriving (Eq,Show)


boolVal "true" = True
boolVal "false" = False

extractString s = (drop 1 (take ((length s)-1) s))

getTokens fn = unsafePerformIO (getTokensAux fn)

getTokensAux fn = do {fh <- openFile fn ReadMode;
                    s <- hGetContents fh;
                    return (alexScanTokens s)}
}
