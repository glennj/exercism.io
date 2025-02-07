module Pangram exposing (isPangram)

import Set exposing (Set)


isPangram : String -> Bool
isPangram sentence =
    let
        letters =
            Set.fromList (String.toList "abcdefghijklmnopqrstuvwxyz")
    in
    Set.isEmpty (removeLetters (String.toLower sentence) letters)


removeLetters : String -> Set Char -> Set Char
removeLetters str letters =
    case ( Set.isEmpty letters, String.uncons str ) of
        ( True, _ ) ->
            letters

        ( _, Nothing ) ->
            letters

        ( _, Just ( char, rest ) ) ->
            removeLetters rest (Set.remove char letters)
