module SecureTreasureChest exposing
    ( Password
    , SecureTreasureChest
    , createPassword
    , createTreasure
    , getTreasure
    )

import String exposing (length)


type Password
    = APassword String


type SecureTreasureChest treasure
    = ASecureTreasureChest String treasure


createPassword : String -> Maybe Password
createPassword passwordCandidate =
    if length passwordCandidate >= 8 then
        Just (APassword passwordCandidate)

    else
        Nothing


createTreasure : a -> Password -> SecureTreasureChest a
createTreasure treasure (APassword password) =
    ASecureTreasureChest password treasure


getTreasure : String -> SecureTreasureChest a -> Maybe a
getTreasure passwordAttempt (ASecureTreasureChest password treasure) =
    if passwordAttempt == password then
        Just treasure

    else
        Nothing
