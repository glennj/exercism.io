module DifferenceOfSquares exposing (difference, squareOfSum, sumOfSquares)


difference : Int -> Int
difference n =
    sumOfSquares n - squareOfSum n |> abs


-- not the most efficient solution, but pretty

squareOfSum : Int -> Int
squareOfSum n =
    sumIt 0 identity n |> square


sumOfSquares : Int -> Int
sumOfSquares n =
    sumIt 0 square n |> identity


sumIt : Int -> (Int -> Int) -> Int -> Int
sumIt sum f n =
    if n == 0 then
        sum
    else
        sumIt (sum + f n) f (n - 1)


square : Int -> Int
square n = n * n
