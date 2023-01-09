module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
    | n < 1     = Nothing
    | n == 1    = Just 0
    | even n    = Just $ maybe 0 (+1) (collatz (n `div` 2))
    | otherwise = Just $ maybe 0 (+1) (collatz (3 * n + 1))
