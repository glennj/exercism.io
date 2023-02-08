module RomanNumerals

// this is not tail recursive, but there shouldn't be that many iterations

let rec roman arabicNumeral = 
    match arabicNumeral with
    | n when n >= 1000 ->  "M" + roman (n - 1000)
    | n when n >=  900 -> "CM" + roman (n - 900)
    | n when n >=  500 ->  "D" + roman (n - 500)
    | n when n >=  400 -> "CD" + roman (n - 400)
    | n when n >=  100 ->  "C" + roman (n - 100)
    | n when n >=   90 -> "XC" + roman (n - 90)
    | n when n >=   50 ->  "L" + roman (n - 50)
    | n when n >=   40 -> "XL" + roman (n - 40)
    | n when n >=   10 ->  "X" + roman (n - 10)
    | n when n >=    9 -> "IX" + roman (n - 9)
    | n when n >=    5 ->  "V" + roman (n - 5)
    | n when n >=    4 -> "IV" + roman (n - 4)
    | n when n >=    1 ->  "I" + roman (n - 1)
    | _ -> ""
