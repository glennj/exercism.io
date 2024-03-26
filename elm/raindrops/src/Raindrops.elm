module Raindrops exposing (raindrops)

import List


raindrops : Int -> String
raindrops num =
    let
        drops : Maybe String
        drops =
            List.foldl
                (\( factor, sound ) sounds ->
                    case ( modBy factor num, sounds ) of
                        ( 0, Nothing ) -> Just sound
                        ( 0, Just ss ) -> Just (ss ++ sound)
                        _ -> sounds
                )
                Nothing
                [ ( 3, "Pling" ), ( 5, "Plang" ), ( 7, "Plong" ) ]
    in
    Maybe.withDefault (String.fromInt num) drops
