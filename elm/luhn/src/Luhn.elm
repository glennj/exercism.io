module Luhn exposing (valid)

import Array exposing (Array)


valid : String -> Bool
valid input =
    let
        cleaned =
            String.replace " " "" input
    in
    if cleaned == "0" then
        False

    else if not (String.all Char.isDigit cleaned) then
        False

    else
        (luhnSum cleaned |> modBy 10) == 0



-- ------------------------------------------------------


luhnSum : String -> Int
luhnSum cleaned =
    let
        zero =
            Char.toCode '0'

        charToInt c =
            Char.toCode c - zero
    in
    cleaned
        |> String.foldr
            (\c ( sum, isDouble ) ->
                ( sum + luhnDigit isDouble (charToInt c)
                , not isDouble
                )
            )
            ( 0, False )
        |> Tuple.first


luhnDigits : Array (Array Int)
luhnDigits =
    Array.fromList
        [ Array.fromList [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
        , Array.fromList [ 0, 2, 4, 6, 8, 1, 3, 5, 7, 9 ]
        ]


luhnDigit : Bool -> Int -> Int
luhnDigit isDouble digit =
    -- I know that my array indexes are in range,
    -- so the Maybe handling can be perfunctory.
    luhnDigits
        |> Array.get (if isDouble then 1 else 0)
        |> Maybe.withDefault Array.empty
        |> Array.get digit
        |> Maybe.withDefault -999
