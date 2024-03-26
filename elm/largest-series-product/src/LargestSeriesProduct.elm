module LargestSeriesProduct exposing (largestProduct)

import List


largestProduct : Int -> String -> Maybe Int
largestProduct length series =
    if length < 1 then
        Nothing

    else
        let
            digitValue char =
                Char.toCode char - Char.toCode '0'

            digits =
                List.map digitValue (String.toList series)
        in
        if List.any (\d -> d < 0 || d > 9) digits then
            Nothing

        else
            List.maximum (products length digits [])


products : Int -> List Int -> List Int -> List Int
products length digits prods =
    if length > List.length digits then
        prods

    else
        products
            length
            (List.drop 1 digits)
            (List.product (List.take length digits) :: prods)
