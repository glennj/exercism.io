module Etl exposing (transform)

import Dict exposing (Dict)


transform : Dict Int (List String) -> Dict String Int
transform input =
    transformer Dict.empty (Dict.toList input)


transformer : Dict String Int -> List (Int, (List String)) -> Dict String Int
transformer transformed pairs =
    case pairs of
        [] -> transformed
        (score, letters) :: rest ->
            transformer (addLetters transformed score letters) rest


addLetters : Dict String Int -> Int -> List String -> Dict String Int
addLetters transformed score letters =
    case letters of 
        [] -> transformed
        letter :: rest ->
            addLetters (Dict.insert (String.toLower letter) score transformed) score rest
