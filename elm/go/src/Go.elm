module Go exposing (..)

import GoSupport exposing (..)


applyRules : Game -> Rule -> NonValidatingRule -> Rule -> Rule -> Game
applyRules game oneStonePerPointRule captureRule libertyRule koRule =
    {- first iteration

        case oneStonePerPointRule game of
            Err e1 ->
                { game | error = e1 }

            Ok g1 ->
                case libertyRule (captureRule g1) of
                    Err e2 ->
                        { game | error = e2 }

                    Ok g2 ->
                        case koRule g2 of
                            Err e3 ->
                                { game | error = e3 }

                            Ok endGame ->
                                changePlayer endGame
    -}
    
    let
        result =
            game
                |> oneStonePerPointRule
                |> Result.map captureRule
                |> Result.andThen libertyRule
                |> Result.andThen koRule
                |> Result.map changePlayer
    in
    case result of
        Err e -> { game | error = e }
        Ok g -> g
