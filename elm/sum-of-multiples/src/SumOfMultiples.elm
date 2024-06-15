module SumOfMultiples exposing (sumOfMultiples)

import Set exposing (Set)


sumOfMultiples : List Int -> Int -> Int
sumOfMultiples divisors limit =
    {- -- The List solution
        List.range 1 (limit - 1)
        |> List.filter (\n -> List.any (\d -> modBy d n == 0) divisors)
        |> List.sum
    -}
    
    -- The solution as described in the instructions
    divisors
    |> List.foldl
        (\divisor multiples ->
            List.range divisor (limit - 1)
            |> List.filter (\n -> modBy divisor n == 0)
            |> Set.fromList
            |> Set.union multiples
        )
        Set.empty
    |> Set.toList
    |> List.sum
