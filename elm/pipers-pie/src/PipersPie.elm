module PipersPie exposing (doubleFactorial, factorial, pipersPi)


factorialHelper : Int -> Int -> Int -> Int
factorialHelper step acc num =
    if num <= 1 then
        acc

    else
        factorialHelper step (acc * num) (num - step)


factorial : Int -> Int
factorial n =
    factorialHelper 1 1 n


doubleFactorial : Int -> Int
doubleFactorial n =
    factorialHelper 2 1 n


pipersPi : Int -> Float
pipersPi limit =
    let
        halfPi : Float -> Int -> Float
        halfPi sum k =
            if k > limit then
                sum

            else
                let
                    numerator = toFloat (factorial k)
                    denominator = toFloat (doubleFactorial (2 * k + 1))
                    newSum = sum + numerator / denominator
                    -- _ = Debug.log "halfPi" ((limit, k), (numerator, denominator), newSum)
                in
                if sum == newSum then
                    {- This converges at 1.5707963267948961 in 50 steps.
                     - Without this check, doubleFactorial overflows and returns Infinity
                     - at step 150, and factorial returns Infinity in 171 steps.
                     - Infinity / Infinity is NaN.
                     -}
                    sum

                else
                    halfPi newSum (k + 1)
    in
    2 * halfPi 0 0
