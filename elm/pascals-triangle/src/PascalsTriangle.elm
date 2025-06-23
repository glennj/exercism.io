module PascalsTriangle exposing (rows)


rows : Int -> List (List Int)
rows n =
    let
        doRows i acc =
            if i > n then
                acc

            else
                let
                    prev =
                        List.head acc |> Maybe.withDefault []
                in
                doRows (i + 1) (row prev :: acc)
    in
    doRows 1 [] |> List.reverse


row : List Int -> List Int
row prevRow =
    let
        doRow prev prevCol acc =
            case prev of
                [] ->
                    1 :: acc

                c :: cs ->
                    doRow cs c ((prevCol + c) :: acc)
    in
    doRow prevRow 0 []
