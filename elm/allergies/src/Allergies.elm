module Allergies exposing (Allergy(..), isAllergicTo, toList)

import Bitwise exposing (and, shiftRightBy)
import List exposing (filter, head, indexedMap, map, member, reverse)
import Tuple exposing (pair, second)


type Allergy
    = Eggs
    | Peanuts
    | Shellfish
    | Strawberries
    | Tomatoes
    | Chocolate
    | Pollen
    | Cats


isAllergicTo : Allergy -> Int -> Bool
isAllergicTo allergy score =
    member allergy (toList score)


toList : Int -> List Allergy
toList score =
    indexedMap pair allergyList
    |> filter (allergic score)
    |> map second


allergic : Int -> ( Int, Allergy ) -> Bool
allergic score ( idx, _ ) =
    and 1 (shiftRightBy idx score) == 1



-- "type iterator pattern"
-- https://sporto.github.io/elm-patterns/basic/type-iterator.html


allergyList : List Allergy
allergyList =
    buildAllergyList []


buildAllergyList : List Allergy -> List Allergy
buildAllergyList list =
    case head list of
        Nothing           -> buildAllergyList (Eggs :: list)
        Just Eggs         -> buildAllergyList (Peanuts :: list)
        Just Peanuts      -> buildAllergyList (Shellfish :: list)
        Just Shellfish    -> buildAllergyList (Strawberries :: list)
        Just Strawberries -> buildAllergyList (Tomatoes :: list)
        Just Tomatoes     -> buildAllergyList (Chocolate :: list)
        Just Chocolate    -> buildAllergyList (Pollen :: list)
        Just Pollen       -> buildAllergyList (Cats :: list)
        Just Cats         -> reverse list
