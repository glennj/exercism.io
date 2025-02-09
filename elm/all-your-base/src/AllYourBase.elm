module AllYourBase exposing (rebase)


rebase : Int -> List Int -> Int -> Maybe (List Int)
rebase inBase digits outBase =
    if inBase < 2 || outBase < 2 then
        Nothing
    else
        toDecimal inBase 0 digits
            |> Maybe.map (toOutBase outBase [])


toDecimal : Int -> Int -> List Int -> Maybe Int
toDecimal inBase acc digits =
    case digits of
        [] ->
            if acc == 0 then
                Nothing
            else
                Just acc

        digit :: rest ->
            if digit < 0 || digit >= inBase then
                Nothing
            else
                toDecimal inBase (inBase * acc + digit) rest


toOutBase : Int -> List Int -> Int -> List Int
toOutBase outBase acc decimal =
    if decimal == 0 then
        acc
    else
        let ( rest, digit ) = divMod outBase decimal
        in  toOutBase outBase (digit :: acc) rest


divMod : Int -> Int -> ( Int, Int )
divMod divisor num =
    ( num // divisor, modBy divisor num )
