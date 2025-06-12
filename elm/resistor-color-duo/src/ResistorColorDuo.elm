module ResistorColorDuo exposing (Color(..), value)

import List.Utils as LU


type Color
    = Black
    | Brown
    | Red
    | Orange
    | Yellow
    | Green
    | Blue
    | Violet
    | Grey
    | White


colorCode : Color -> Int
colorCode color =
    case color of
        Black -> 0
        Brown -> 1
        Red -> 2
        Orange -> 3
        Yellow -> 4
        Green -> 5
        Blue -> 6
        Violet -> 7
        Grey -> 8
        White -> 9


value : List Color -> Int
value colors =
    let
        codes = colors |> LU.take 2 |> List.map colorCode
    in
    case codes of
        first :: second :: _ -> 10 * first + second
        _ -> 0
