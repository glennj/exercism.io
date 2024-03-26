module MagicianInTraining exposing (..)

-- have to explicitly expose the Array type

import Array exposing (Array)


getCard : Int -> Array Int -> Maybe Int
{-
getCard index deck =
    Array.get index deck

-- because the args are used in the same order, we can
-- use a partial function
-}
getCard =
    Array.get


setCard : Int -> Int -> Array Int -> Array Int
{-
setCard index newCard deck =
    Array.set index newCard deck
-}
setCard =
    Array.set

addCard : Int -> Array Int -> Array Int
{-
addCard newCard deck =
    Array.push newCard deck
-}
addCard =
    Array.push

removeCard : Int -> Array Int -> Array Int
removeCard index deck =
    Array.append
        (Array.slice 0 index deck)
        (Array.slice (index + 1) (Array.length deck) deck)


evenCardCount : Array Int -> Int
evenCardCount deck =
    let
        isEven num =
            modBy 2 num == 0

        increment card count =
            count + (if isEven card then 1 else 0)
    in
    Array.foldl increment 0 deck
