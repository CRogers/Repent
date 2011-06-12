module Tokeniser (Token(Const, VarName, Symbol), isConst, tokenise) where

import Char

data Token = Const {tconst::Integer} | VarName Char | Symbol Char

isConst (Const _) = True
isConst _ = False

tokenise :: Token -> String -> [Token]
tokenise prev [] = [prev]
tokenise prev (c:cs)
    | any (==c) " \t\r\n" = prev : tokenise (Symbol ' ') cs
    | isDigit c = let n = charToDigit c in
                    if isConst prev then let con = Const $ n + 10 * tconst prev in tokenise con cs
                                    else prev : tokenise (Const n) cs
    | isUpper c = prev : tokenise (VarName c) cs
    | otherwise = prev : tokenise (Symbol c) cs

charToDigit :: Char -> Integer
charToDigit c = read [c]
