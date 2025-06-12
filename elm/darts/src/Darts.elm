module Darts exposing (score)


score : Float -> Float -> Int
score x y =
    let
        distance = x ^ 2 + y ^ 2 |> sqrt
    in
    if distance <= 1 then
        10
    else if distance <= 5 then
        5
    else if distance <= 10 then
        1
    else
        0
