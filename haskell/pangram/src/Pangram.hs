module Pangram (isPangram) where

import Data.Char (isAlpha, toLower)
import qualified Data.Set as Set

isPangram :: String -> Bool
isPangram text = 
    let set = Set.fromList $ map toLower $ filter isAlpha text
        alphabet = ['a'..'z']
    in Set.size set == length alphabet