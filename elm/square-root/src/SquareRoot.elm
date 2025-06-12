module SquareRoot exposing (squareRoot)

-- Using the Binary numeral system (base 2) from Wikipedia
-- https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Binary_numeral_system_%28base_2%29


squareRoot : Int -> Int
squareRoot radicand =
    let
        -- largest power of 4 less than or equal to the radicand
        bInit =
            4 ^ (radicand |> toFloat |> logBase 4 |> floor)

        sqrt n b x =
            if b == 0 then
                x

            else if n >= x + b then
                sqrt (n - x - b) (b // 4) (x // 2 + b)

            else
                sqrt n (b // 4) (x // 2)
    in
    sqrt radicand bInit 0
