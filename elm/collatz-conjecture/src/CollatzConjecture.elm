module CollatzConjecture exposing (collatz)


collatz : Int -> Result String Int
collatz start =
    if start <= 0 then
        Err "Only positive integers are allowed"

    else
        Ok (helper 0 start)


helper : Int -> Int -> Int
helper steps num =
    if num == 1 then
        steps

    else
        helper (steps + 1) (nextNum num)


nextNum : Int -> Int
nextNum num =
    if isEven num then
        num // 2

    else
        num * 3 + 1


isEven : Int -> Bool
isEven num =
    modBy 2 num == 0
