module TopScorers exposing (..)

import Dict exposing (Dict)
import List
import TopScorersSupport exposing (PlayerName)


type alias GoalCounts =
    Dict PlayerName Int


updateGoalCountForPlayer : PlayerName -> GoalCounts -> GoalCounts
updateGoalCountForPlayer playerName playerGoalCounts =
    Dict.update
        playerName
        (\goals -> Just (1 + Maybe.withDefault 0 goals))
        playerGoalCounts

    {- or,
        Dict.insert
            playerName
            (1 + getGoalCountForPlayer playerName playerGoalCounts)
            playerGoalCounts
    -}


getGoalCountForPlayer : PlayerName -> GoalCounts -> Int
getGoalCountForPlayer playerName playerGoalCounts =
    Maybe.withDefault 0 (Dict.get playerName playerGoalCounts)


aggregateScorers : List PlayerName -> GoalCounts
aggregateScorers =
    List.foldl updateGoalCountForPlayer Dict.empty


removeInsignificantPlayers : Int -> GoalCounts -> GoalCounts
removeInsignificantPlayers goalThreshold =
    Dict.filter (\_ goals -> goals >= goalThreshold)


resetPlayerGoalCount : PlayerName -> GoalCounts -> GoalCounts
resetPlayerGoalCount playerName =
    Dict.insert playerName 0


formatPlayer : PlayerName -> GoalCounts -> String
formatPlayer playerName playerGoalCounts =
    formatPlayerWithGoals
        playerName
        (getGoalCountForPlayer playerName playerGoalCounts)


formatPlayerWithGoals : PlayerName -> Int -> String
formatPlayerWithGoals playerName goals =
    playerName ++ ": " ++ String.fromInt goals


formatPlayers : GoalCounts -> String
formatPlayers =
    Dict.map formatPlayerWithGoals
        >> Dict.values
        >> String.join ", "


combineGames : GoalCounts -> GoalCounts -> GoalCounts
combineGames game1 game2 =
    let
        inOne =
            Dict.insert

        inBoth name goals1 goals2 =
            Dict.insert name (goals1 + goals2)
    in
    Dict.merge inOne inBoth inOne game1 game2 Dict.empty
