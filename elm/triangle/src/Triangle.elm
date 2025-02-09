module Triangle exposing (Triangle(..), triangleKind)

import Set


type Triangle
    = Equilateral
    | Isosceles
    | Scalene


triangleKind : number -> number -> number -> Result String Triangle
triangleKind x y z =
    isValid x y z |> Result.map getKind


isValid : number -> number -> number -> Result String (List number)
isValid x y z =
    if x <= 0 || y <= 0 || z <= 0 then
        Err "Invalid lengths"

    else if x + y <= z || x + z <= y || y + z <= x then
        Err "Violates inequality"

    else
        Ok [ x, y, z ]


getKind : List number -> Triangle
getKind sides =
    let
        uniqueSides = sides |> Set.fromList |> Set.size
    in
    case uniqueSides of
        1 -> Equilateral
        2 -> Isosceles
        _ -> Scalene
