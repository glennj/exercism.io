module Wordy exposing (answer)

import Regex exposing (fromString, contains)


answer : String -> Maybe Int
answer problem =
    let
        overall =
            Regex.fromString "^What is (\\d+)\\s*(.+)?\\?$"
                |> Maybe.withDefault Regex.never
    in
    case Regex.find overall problem of
        [m] -> 
            let _ = Debug.log "found match" m
            in
            case m.submatches of
                [(Just init), expr] ->
                    String.toInt init |> Maybe.map (solve expr)
                _ ->
                    Nothing
        _ -> 
            Nothing |> Debug.log "overall not matched"


solve : Maybe String -> Int -> Int
solve expr acc =
    let
        ops = "(plus|minus|multiplied by|divided by)"
        operations = 
            Regex.fromString ("\\s+" ++ ops ++ "\\s+(\\d+)")
                |> Maybe.withDefault Regex.never
    in
    case expr of
        Nothing ->
            acc
        _ ->
            acc
