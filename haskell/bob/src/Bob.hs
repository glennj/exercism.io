module Bob (responseFor) where

import Data.Char (isAlpha, isLower, isSpace)

responseFor :: String -> String
responseFor xs
    | isShoutedQuestion xs = "Calm down, I know what I'm doing!"
    | isShouting xs        = "Whoa, chill out!"
    | isQuestion xs        = "Sure."
    | isSilent xs          = "Fine. Be that way!"
    | otherwise            = "Whatever."

isShoutedQuestion :: String -> Bool
isShoutedQuestion xs = isShouting xs && isQuestion xs

-- a string is shouting if it contains a letter but no lower case letters
isShouting :: String -> Bool
isShouting xs = contains isAlpha xs && not (contains isLower xs)

contains :: (Char -> Bool) -> String -> Bool
contains _ [] = False
contains f (x:xs)
    | f x       = True
    | otherwise = contains f xs

isQuestion :: String -> Bool
isQuestion xs = isQuestionR (reverse xs)
    
isQuestionR :: String -> Bool
isQuestionR [] = False
isQuestionR (x:xs)
    | x == '?'  = True
    | isSpace x = isQuestionR xs
    | otherwise = False

isSilent :: String -> Bool
isSilent [] = True
isSilent (x:xs)
    | isSpace x = isSilent xs
    | otherwise = False
