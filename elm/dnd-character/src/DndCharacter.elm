module DndCharacter exposing (Character, ability, character, modifier)

import Random exposing (Generator)


type alias Character =
    { strength : Int
    , dexterity : Int
    , constitution : Int
    , intelligence : Int
    , wisdom : Int
    , charisma : Int
    , hitpoints : Int
    }


modifier : Int -> Int
modifier score =
    toFloat (score - 10) / 2 |> floor


ability : Generator Int
ability =
    Random.list 4 (Random.int 1 6)
        |> Random.map (\dice -> List.sum dice - (List.minimum dice |> Maybe.withDefault 0))


character : Generator Character
character =
    {- the type alias `Character` is actual a function:
       Character : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Character

       We use the Random.map* function to replace the Int arguments with
       (Generator Int) arguments to produce a (Generator Character)

       Ref https://github.com/elm-community/random-extra/blob/d52055975644ad401709c2aff14dab9ca93e44a0/src/Random/Extra.elm#L62
    -}
    Random.map Character ability -- strength
        |> Random.map2 (|>) ability -- dexterity
        |> Random.map2 (|>) ability -- constitution
        |> Random.map2 (|>) ability -- intelligence
        |> Random.map2 (|>) ability -- wisdom
        |> Random.map2 (|>) ability -- charisma
        |> Random.map2 (|>) (Random.constant 0) -- placeholder for hitpoints
        |> Random.map (\c -> { c | hitpoints = 10 + modifier c.constitution })
