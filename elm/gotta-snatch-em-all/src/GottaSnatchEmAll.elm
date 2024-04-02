module GottaSnatchEmAll exposing (..)

import Set exposing (Set)


type alias Card =
    String


newCollection : Card -> Set Card
newCollection =
    Set.singleton


addCard : Card -> Set Card -> ( Bool, Set Card )
addCard card collection =
    ( Set.member card collection, Set.insert card collection )


tradeCard : Card -> Card -> Set Card -> ( Bool, Set Card )
tradeCard yourCard theirCard collection =
    let
        gotIt card =
            Set.member card collection
    in
    ( gotIt yourCard && not (gotIt theirCard)
    , collection |> Set.remove yourCard |> Set.insert theirCard
    )


removeDuplicates : List Card -> List Card
removeDuplicates =
    Set.fromList >> Set.toList


extraCards : Set Card -> Set Card -> Int
extraCards yourCollection theirCollection =
    theirCollection
        |> Set.diff yourCollection
        |> Set.size


boringCards : List (Set Card) -> List Card
boringCards collections =
    case collections of
        [] ->
            []

        first :: rest ->
            List.foldl Set.intersect first rest |> Set.toList


totalCards : List (Set Card) -> Int
totalCards =
    List.foldl Set.union Set.empty >> Set.size


splitShinyCards : Set Card -> ( List Card, List Card )
splitShinyCards =
    Set.partition (String.startsWith "Shiny")
        >> Tuple.mapBoth Set.toList Set.toList
