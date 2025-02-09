module Yacht exposing (Category(..), score)

import Set


type Category
    = Ones
    | Twos
    | Threes
    | Fours
    | Fives
    | Sixes
    | FullHouse
    | FourOfAKind
    | LittleStraight
    | BigStraight
    | Choice
    | Yacht


score : List Int -> Category -> Int
score dice category =
    case category of
        Ones   -> single 1 dice
        Twos   -> single 2 dice
        Threes -> single 3 dice
        Fours  -> single 4 dice
        Fives  -> single 5 dice
        Sixes  -> single 6 dice

        FullHouse      -> full dice
        FourOfAKind    -> four dice
        LittleStraight -> straight [ 1, 2, 3, 4, 5 ] dice
        BigStraight    -> straight [ 2, 3, 4, 5, 6 ] dice
        Choice         -> List.sum dice
        Yacht          -> yacht dice


single die dice =
    List.filter (\d -> d == die) dice |> List.sum


full dice =
    case List.sort dice of
        [ a, b, c, d, e ] ->
            let isYacht = a == e
                isFull = (a == b && c == e) || (a == c && d == e)
            in
                if not isYacht && isFull then List.sum dice else 0
        _ -> 0


four dice =
    case List.sort dice of
        [ a, b, c, d, e ] ->
            if a == d || b == e then 4 * c else 0
        _ -> 0


straight wanted dice =
    if wanted == List.sort dice then 30 else 0


yacht dice =
    if Set.size (Set.fromList dice) == 1 then 50 else 0
