module List.Utils exposing (..)


take : Int -> List a -> List a
take n list =
    let
        takeRec elems taken =
            case elems of
                [] ->
                    taken

                x :: xs ->
                    if List.length taken == n then
                        taken

                    else
                        takeRec xs (taken ++ [ x ])
    in
    takeRec list []


at : Int -> List a -> Maybe a
at n list =
    case list of
        [] -> Nothing
        x :: xs ->
            if n == 0 then
                Just x
            else
                at (n - 1) xs
