module Pangram (isPangram) where

import Data.Char (isAlpha, isAscii, toLower)
import Data.List (nub)

isPangram :: String -> Bool
isPangram text = 
    let alphabet = ['a'..'z']
    {-
        letters      = filter isAlpha text
        asciiLetters = filter isAscii letters
        lowers       = map toLower asciiLetters
        uniqLetters  = nub lowers
    -}
    {-
        uniqLetters = (nub
                       . map toLower
                       . filter isAscii
                       . filter isAlpha
                      ) text
    -}
        uniqLetters = nub
                      $ map toLower
                      $ filter isAscii
                      $ filter isAlpha text
    in length uniqLetters == length alphabet
