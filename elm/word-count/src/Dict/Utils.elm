module Dict.Utils exposing (..)

import Dict exposing (Dict)


incr : comparable -> Dict comparable Int -> Dict comparable Int
incr key dict =
    Dict.update key (\val -> 1 + Maybe.withDefault 0 val |> Just) dict
