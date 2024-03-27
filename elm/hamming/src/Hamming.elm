module Hamming exposing (distance)

import List


distance : String -> String -> Result String Int
distance left right =
    if String.length left /= String.length right then
        Err "strands must be of equal length"

    else
        Ok  (List.map2 (==) (String.toList left) (String.toList right)
            |> List.foldl (\eq sum -> sum + if eq then 0 else 1) 0
            )
