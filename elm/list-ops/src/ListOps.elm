module ListOps exposing
    ( append
    , concat
    , filter
    , foldl
    , foldr
    , length
    , map
    , reverse
    )

{-
   Attempting to do this using as little builtin functionality as possible
   - `[]` to create an empty list
   - `::` to "cons" an element onto the head of a list
   
   Once have `foldl`, everything else follows.
-}


foldl : (a -> b -> b) -> b -> List a -> b
foldl f initial list =
    let
        doFoldl acc l =
            case l of
                [] -> acc
                x :: xs -> doFoldl (f x acc) xs
    in
    doFoldl initial list


length : List a -> Int
length list =
    foldl (\_ count -> count + 1) 0 list


reverse : List a -> List a
reverse list =
    foldl (::) [] list


foldr : (a -> b -> b) -> b -> List a -> b
foldr f acc list =
    foldl f acc (reverse list)


append : List a -> List a -> List a
append listA listB =
    foldl (::) listB (reverse listA)


map : (a -> b) -> List a -> List b
map f list =
    foldl (\x acc -> f x :: acc) [] list |> reverse


filter : (a -> Bool) -> List a -> List a
filter f list =
    foldl (\x acc -> if f x then x :: acc else acc) [] list |> reverse


concat : List (List a) -> List a
concat list =
    foldl append [] (reverse list)
