module TreasureFactory exposing (TreasureChest, getTreasure, makeChest, makeTreasureChest, secureChest, uniqueTreasures)

import List
import String


type TreasureChest treasure
    = TreasureChest String treasure


getTreasure : String -> TreasureChest a -> Maybe a
getTreasure passwordAttempt (TreasureChest password treasure) =
    if passwordAttempt == password then
        Just treasure

    else
        Nothing


type Chest treasure conditions
    = Chest String treasure


makeChest : String -> treasure -> Chest treasure {}
makeChest password treasure =
    Chest password treasure


secureChest : Chest treasure conditions -> Maybe (Chest treasure { conditions | securePassword : () })
secureChest (Chest password treasure) =
    if String.length password >= 8 then
        Just (Chest password treasure)

    else
        Nothing


uniqueTreasures : List (Chest treasure conditions) -> List (Chest treasure { conditions | uniqueTreasure : () })
uniqueTreasures listOfChests =
    let
        treasures =
            List.map (\(Chest _ treasure) -> treasure) listOfChests

        -- I'd create a dict mapping treasure to count, but because
        --   1. `treasure` is generic, and
        --   2. dict keys need to be `comparable`
        -- I can't do that.
        isUnique treasure =
            List.length (List.filter (\t -> t == treasure) treasures) == 1

        makeUniqChest : Chest treasure conditions -> Chest treasure { conditions | uniqueTreasure : () }
        makeUniqChest (Chest password treasure) =
            Chest password treasure
    in
    listOfChests
        |> List.filter (\(Chest _ treasure) -> isUnique treasure)
        |> List.map makeUniqChest


makeTreasureChest : Chest treasure { conditions | securePassword : (), uniqueTreasure : () } -> TreasureChest treasure
makeTreasureChest (Chest password treasure) =
    TreasureChest password treasure
