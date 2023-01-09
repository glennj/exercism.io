module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA "" = Right ""
toRNA (x:xs)
    | x == 'C' = either (id) (\b -> Right ('G':b)) (toRNA xs)
    | x == 'G' = either (id) (\b -> Right ('C':b)) (toRNA xs)
    | x == 'T' = either (id) (\b -> Right ('A':b)) (toRNA xs)
    | x == 'A' = either (id) (\b -> Right ('U':b)) (toRNA xs)
    | otherwise = Left x
