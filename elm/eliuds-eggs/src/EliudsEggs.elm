module EliudsEggs exposing (eggCount)

import Bitwise exposing (and, shiftRightBy)


eggCount : Int -> Int
eggCount n =
    eggCounter n 0


eggCounter number count =
    case number of
        0 -> count
        _ -> eggCounter (shiftRightBy 1 number) (and number 1 + count)
