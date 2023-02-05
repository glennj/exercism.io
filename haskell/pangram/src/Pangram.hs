module Pangram (isPangram) where

import Data.Char (isAlpha, isAscii, toLower)

isPangram :: String -> Bool
isPangram text = isPangramRec text []

isPangramRec :: String -> [Char] -> Bool
isPangramRec "" seen = length seen == length ['a'..'z']
isPangramRec (c:cs) seen
  | isAscii c && isAlpha c && not (lc `elem` seen)
      = isPangramRec cs (seen ++ [lc])
  | otherwise = isPangramRec cs seen
  where lc = toLower c

{--
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
--}
