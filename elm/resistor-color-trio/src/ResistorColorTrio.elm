module ResistorColorTrio exposing (Color(..), label)


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
    case colors of
        first :: second :: _ -> 10 * (colorCode first) + colorCode second
        _ -> 0


label : List Color -> String
label colors =
    case colors of
        band1 :: band2 :: band3 :: _ ->
            let
                resistance = (value [band1, band2]) * 10 ^ (colorCode band3)

                reduce r i =
                    if r == 0 || (modBy 1000 r) /= 0 then
                        (r, i)
                    else
                        reduce (r // 1000) (i + 1)

                (labelValue, idx) = reduce resistance 0

                prefix = case idx of
                    1 -> "kilo"
                    2 -> "mega"
                    3 -> "giga"
                    _ -> ""
            in
            (String.fromInt labelValue) ++ " " ++ prefix ++ "ohms"

        _ ->
            ""
