module RomanNumerals exposing (toRoman)


toRoman : Int -> String
toRoman number =
    if      number >= 1000 then  "M" ++ toRoman (number - 1000)
    else if number >=  900 then "CM" ++ toRoman (number - 900)
    else if number >=  500 then  "D" ++ toRoman (number - 500)
    else if number >=  400 then "CD" ++ toRoman (number - 400)
    else if number >=  100 then  "C" ++ toRoman (number - 100)
    else if number >=   90 then "XC" ++ toRoman (number - 90)
    else if number >=   50 then  "L" ++ toRoman (number - 50)
    else if number >=   40 then "XL" ++ toRoman (number - 40)
    else if number >=   10 then  "X" ++ toRoman (number - 10)
    else if number >=    9 then "IX" ++ toRoman (number - 9)
    else if number >=    5 then  "V" ++ toRoman (number - 5)
    else if number >=    4 then "IV" ++ toRoman (number - 4)
    else if number >=    1 then  "I" ++ toRoman (number - 1)
    else ""
