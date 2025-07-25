module TreasureChest exposing (..)


type TreasureChest treasureType
    = TreasureChest String treasureType


getTreasure : String -> TreasureChest a -> Maybe a
getTreasure passwordAttempt (TreasureChest password treasure) =
    if passwordAttempt == password then
        Just treasure

    else
        Nothing


multiplyTreasure : (a -> List a) -> TreasureChest a -> TreasureChest (List a)
multiplyTreasure multiplier (TreasureChest password treasure) =
    TreasureChest password (multiplier treasure)
