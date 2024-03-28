module BlorkemonCards exposing
    ( Card
    , compareShinyPower
    , expectedWinner
    , isMorePowerful
    , maxPower
    , sortByCoolness
    , sortByMonsterName
    )

import List


type alias Card =
    { monster : String, power : Int, shiny : Bool }


isMorePowerful : Card -> Card -> Bool
isMorePowerful card1 card2 =
    card1.power > card2.power


maxPower : Card -> Card -> Int
maxPower card1 card2 =
    max card1.power card2.power


sortByMonsterName : List Card -> List Card
sortByMonsterName =
    List.sortBy .monster


sortByCoolness : List Card -> List Card
sortByCoolness =
    -- Bools aren't comparable, have to "convert" to a comparable type.
    -- Both properties are sorted in descending order.
    List.sortBy (\card -> ( -(boolToComparable card.shiny), -card.power ))


boolToComparable : Bool -> Int
boolToComparable b =
    -- value judgement: True is greater than False
    if b then 1 else 0


compareShinyPower : Card -> Card -> Order
compareShinyPower card1 card2 =
    let
        cmp card =
            ( card.power, boolToComparable card.shiny )
    in
    compare (cmp card1) (cmp card2)


expectedWinner : Card -> Card -> String
expectedWinner card1 card2 =
    case compareShinyPower card1 card2 of
        LT -> card2.monster
        GT -> card1.monster
        EQ -> "too close to call"
