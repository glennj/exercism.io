module Series exposing (slices)


slices : Int -> String -> Result String (List (List Int))
slices size input =
    if String.isEmpty input then
        Err "series cannot be empty"

    else if size > String.length input then
        Err "slice length cannot be greater than series length"

    else if size == 0 then
        Err "slice length cannot be zero"

    else if size < 0 then
        Err "slice length cannot be negative"

    else
        Ok (getSlices size input [] |> List.map toDigits)


getSlices : Int -> String -> List String -> List String
getSlices size str substrs =
    if String.length str < size then
        List.reverse substrs

    else
        getSlices size
            (String.dropLeft 1 str)
            (String.left size str :: substrs)


toDigits : String -> List Int
toDigits str =
    let zeroCode = Char.toCode '0'
    in
    -- assuming every char is a digit
    str |> String.toList |> List.map (\c -> Char.toCode c - zeroCode)
