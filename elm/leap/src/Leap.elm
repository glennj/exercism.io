module Leap exposing (isLeapYear)


isLeapYear : Int -> Bool
isLeapYear year =
    let
        divBy divisor = (modBy divisor year) == 0
    in
    divBy 4 && not (divBy 100) || divBy 400
