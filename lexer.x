{
module Lexer (Token(..), AlexPosn(..), alexScanTokens, getTokens) where

import System.IO
import System.IO.Unsafe
}



%wrapper "posn"

$digit = 0-9          -- digits
$alpha = [a-zA-Z]     -- alphabetic characters

tokens :-

----------------------- FLOW  --------------------------
    if                                      { \p s -> If p }
    then                                    { \p s -> Then p }
    else                                    { \p s -> Else p }
    while                                   { \p s -> While p }
    for                                     { \p s -> For p }
    continue                                { \p s -> Continue p }
    break                                   { \p s -> Break p }

---------------------- SCOPES  -------------------------

    \(                                      { \p s -> LeftParen p }
    \)                                      { \p s -> RightParen p }
    \[                                      { \p s -> LeftBracket p }
    \]                                      { \p s -> RightBracket p }
    \{                                      { \p s -> LeftBrace p }
    \}                                      { \p s -> RightBrace p }

--------------------- OPERADORES  --------------------------

    :=                                      { \p s -> Assign p }
    "%"                                     { \p s -> Mod p }
    \^                                      { \p s -> Expo p }
    \+                                      { \p s -> Plus p }
    \-                                      { \p s -> Minus p }
    \*                                      { \p s -> Times p }
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

    \-?$digit+\.$digit+                     { \p s -> Double p (read s) }
    \-?$digit+                              { \p s -> Int p (read s) }
    "true"                                  { \p s -> Bool p (read s) }
    "false"                                 { \p s -> Bool p (read s) }
    \"$alpha [$alpha $digit ! \_ \']*\"     { \p s -> String p s }

-------------------------- MAIN ------------------------------------
    $white+                                 ;
    "//".*                                  ; -- ignora comentários
    ("Int" | "Double" | "Bool" | "String")  { \p s -> Type p s }
    return                                  { \p s -> Return p }
    import                                  { \p s -> Import p }
    func                                    { \p s -> Func p }
    proc                                    { \p s -> Proc p }
    $alpha [$alpha $digit \_ \']*           { \p s -> Id p s }
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
-- SCOPES  ---------------------------------------------
    LeftBrace AlexPosn |
    RightBrace AlexPosn |
    LeftParen AlexPosn |
    RightParen AlexPosn |
    LeftBracket AlexPosn |
    RightBracket AlexPosn |
-- OPERADORES  -----------------------------------------
    Assign AlexPosn |
    Mod  AlexPosn |
    Expo AlexPosn |
    Plus AlexPosn |
    Minus AlexPosn |
    Times AlexPosn |
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
    Double AlexPosn Double |
    Int AlexPosn Int |
    Bool AlexPosn Bool |
    String AlexPosn String |
-- MAIN  ---------------------------------------------
    Type AlexPosn String |
    Id AlexPosn String |
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



getTokens fn = unsafePerformIO (getTokensAux fn)

getTokensAux fn = do {fh <- openFile fn ReadMode;
                    s <- hGetContents fh;
                    return (alexScanTokens s)}
}
