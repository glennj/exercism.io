module Isogram exposing (isIsogram)

import Set exposing (Set)


isIsogram : String -> Bool
isIsogram sentence =
    helper (String.toLower sentence) Set.empty


helper : String -> Set Char -> Bool
helper str seen =
    case String.uncons str of
        Nothing ->
            True

        Just ( letter, rest ) ->
            case ( Char.isAlpha letter, Set.member letter seen ) of
                ( True, True ) ->
                    False

                ( True, False ) ->
                    helper rest (Set.insert letter seen)

                _ ->
                    helper rest seen
