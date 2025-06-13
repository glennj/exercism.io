module PythagoreanTriplet exposing (triplets)


type alias Triplet =
    ( Int, Int, Int )


triplets : Int -> List Triplet
triplets n =
    let
        findTriplets a trips =
            let
                -- derived from
                --   a + b + c = n
                --   a^2 + b^2 = c^2
                b = n * (n - 2 * a) // (2 * (n - a))
                c = n - a - b
            in
            if a >= b then
                trips |> List.reverse

            else if a^2 + b^2 == c^2 then
                findTriplets (a + 1) (( a, b, c ) :: trips)

            else
                findTriplets (a + 1) trips
    in
    findTriplets 3 []
