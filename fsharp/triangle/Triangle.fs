module Triangle

type Triangle = float list


let valid (triangle: Triangle) =
    match List.sort triangle with
    | [a; b; c] -> a > 0 && a + b >= c
    | _ -> false

let numDistinctSides (triangle: Triangle) =
    triangle |> List.distinct |> List.length


let equilateral (triangle: Triangle) = 
    valid triangle && numDistinctSides triangle = 1

let isosceles (triangle: Triangle) = 
    valid triangle && numDistinctSides triangle < 3

let scalene (triangle: Triangle) = 
    valid triangle && numDistinctSides triangle = 3
