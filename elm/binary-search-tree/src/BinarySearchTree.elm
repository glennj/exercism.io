module BinarySearchTree exposing (BinaryTree(..), makeTree, sort)


type BinaryTree
    = Empty
    | Tree BinaryTree Int BinaryTree


makeTree : List Int -> BinaryTree
makeTree data =
    let
        insert value tree =
            case tree of
                Empty ->
                    Tree Empty value Empty

                Tree left v right ->
                    if value <= v then
                        Tree (insert value left) v right

                    else
                        Tree left v (insert value right)
    in
    List.foldl insert Empty data


sort : List Int -> List Int
sort data =
    let
        walk tree =
            case tree of
                Empty ->
                    []

                Tree left value right ->
                    walk left ++ [ value ] ++ walk right
    in
    makeTree data |> walk
