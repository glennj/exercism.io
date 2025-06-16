module Say exposing (SayError(..), say)


type SayError
    = Negative
    | TooLarge


say : Int -> Result SayError String
say number =
    if number < 0 then
        Err Negative

    else if number > 999999999999 then
        Err TooLarge

    else
        Ok (sayIt number |> lastAnd)


sayIt number =
    if number < 20 then
        saySmall number

    else if number < 100 then
        sayDoubleDigits number

    else if number < 1000 then
        sayCompound number 100 "hundred"

    else if number < 1000000 then
        sayCompound number 1000 "thousand"

    else if number < 1000000000 then
        sayCompound number 1000000 "million"

    else if number < 1000000000000 then
        sayCompound number 1000000000 "billion"

    else
        ""


saySmall number =
    case number of
        0 -> "zero"
        1 -> "one"
        2 -> "two"
        3 -> "three"
        4 -> "four"
        5 -> "five"
        6 -> "six"
        7 -> "seven"
        8 -> "eight"
        9 -> "nine"
        10 -> "ten"
        11 -> "eleven"
        12 -> "twelve"
        13 -> "thirteen"
        14 -> "fourteen"
        15 -> "fifteen"
        16 -> "sixteen"
        17 -> "seventeen"
        18 -> "eighteen"
        19 -> "nineteen"
        _ -> ""


sayTens number =
    case number of
        2 -> "twenty"
        3 -> "thirty"
        4 -> "forty"
        5 -> "fifty"
        6 -> "sixty"
        7 -> "seventy"
        8 -> "eighty"
        9 -> "ninety"
        _ -> ""


divMod : Int -> Int -> ( Int, Int )
divMod base number =
    ( number // base, modBy base number )


sayDoubleDigits number =
    let
        ( q, r ) = divMod 10 number
    in
    sayTens q
        ++  if r > 0 then
                "-" ++ saySmall r
            else
                ""


sayCompound number base name =
    let
        ( q, r ) = divMod base number
    in
    sayIt q
        ++ " "
        ++ name
        ++  if r > 0 then
                (if name == "hundred" then " and" else "")
                    ++ " "
                    ++ sayIt r
            else
               ""


lastAnd str =
    case List.reverse (String.split " " str) of
        [] -> str
        _ :: [] -> str
        "hundred" :: _ -> str
        "thousand" :: _ -> str
        "million" :: _ -> str
        "billion" :: _ -> str
        _ :: "and" :: _ -> str
        head :: tail ->
            String.join " " (List.reverse tail ++ [ "and", head ])
