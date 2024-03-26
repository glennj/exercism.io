module ArmstrongNumbers exposing (isArmstrongNumber)


isArmstrongNumber : Int -> Bool
isArmstrongNumber num =
    num == aliquotSum num


aliquotSum num =
    let
        len = 1 + floor (logBase 10 (toFloat num))

        doSum n sum =
            case n of
                0 -> sum
                _ -> doSum (n // 10) (sum + modBy 10 n ^ len)
    in
    doSum num 0
