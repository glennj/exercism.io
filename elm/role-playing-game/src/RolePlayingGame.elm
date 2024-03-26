module RolePlayingGame exposing (Player, castSpell, introduce, revive)


type alias Player =
    { name : Maybe String
    , level : Int
    , health : Int
    , mana : Maybe Int
    }


introduce : Player -> String
introduce { name } =
    Maybe.withDefault "Mighty Magician" name


revive : Player -> Maybe Player
revive ({ health, level } as player) =
    case ( health, level >= 10 ) of
        ( 0, True ) ->
            Just { player | health = 100, mana = Just 100 }

        ( 0, False ) ->
            Just { player | health = 100, mana = Nothing }

        _ ->
            Nothing


castSpell : Int -> Player -> ( Player, Int )
castSpell manaCost ({ mana, health } as player) =
    case Maybe.map (\m -> ( m >= manaCost, m )) mana of
        Nothing ->
            -- player's mana is Nothing, penalize health
            ( { player | health = max 0 (health - manaCost) }, 0 )

        Just ( False, _ ) ->
            -- not enough mana
            ( player, 0 )

        Just ( True, m ) ->
            ( { player | mana = Just (m - manaCost) }
            , 2 * manaCost
            )


{- take 1
revive : Player -> Maybe Player
revive player =
    if player.health > 0 then
        Nothing
    else if player.level >= 10 then
        Just { player | health = 100, mana = Just 100 }
    else
        Just { player | health = 100, mana = Nothing }

castSpell : Int -> Player -> ( Player, Int )
castSpell manaCost player =
    case player.mana of
        Nothing ->
            let
                newHealth =
                    max 0 (player.health - manaCost)
            in
            ( { player | health = newHealth }, 0 )
        Just mana ->
            if mana < manaCost then
                ( player, 0 )
            else
                ( { player | mana = Just (mana - manaCost) }
                , 2 * manaCost
                )
-}
