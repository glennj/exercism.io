module Grains exposing (square)

import Bitwise


square : Int -> Maybe Int
square n =
    if n <= 0 then
        Nothing

    else
        -- Just (2 ^ (n - 1))
        Just (1 |> Bitwise.shiftLeftBy (n - 1) |> abs)

{- An oddity here:
        > 2 ^ 31
        2147483648 : number
   but
        > 1 |> Bitwise.shiftLeftBy 31
        -2147483648 : Int
        > 1 |> Bitwise.shiftLeftBy 31 |> abs
        2147483648 : Int
        
-}
