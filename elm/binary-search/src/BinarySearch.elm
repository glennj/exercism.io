module BinarySearch exposing (find)

import Array exposing (Array)


find : Int -> Array Int -> Maybe Int
find target xs =
    finder target xs {left = 0, right = (Array.length xs - 1)}


finder target xs ({left, right} as indices) =
    if left > right then
        -- target not found
        Nothing

    else
        let
            mid = (left + right) // 2
        in
        case Array.get mid xs of
            Nothing -> Nothing   -- should not happen
            Just value ->
                case compare target value of
                    EQ -> Just mid
                    LT -> finder target xs {indices | right = (mid - 1)}
                    GT -> finder target xs {indices | left = (mid + 1)}
