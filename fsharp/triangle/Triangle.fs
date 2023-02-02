module Triangle

type Triangle = float list


let valid (triangle: Triangle) =
    let sides = triangle |> List.sort |> List.toArray
    let a, b, c = sides[0], sides[1], sides[2]
    a > 0 && a + b >= c

let numDistinctSides (triangle: Triangle) =
    triangle |> List.distinct |> List.length


let equilateral (triangle: Triangle) = 
    valid triangle && numDistinctSides triangle = 1

let isosceles (triangle: Triangle) = 
    valid triangle && numDistinctSides triangle < 3

let scalene (triangle: Triangle) = 
    valid triangle && numDistinctSides triangle = 3
