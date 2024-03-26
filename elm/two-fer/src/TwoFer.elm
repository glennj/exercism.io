module TwoFer exposing (twoFer)


twoFer : Maybe String -> String
twoFer name =
    let
        who = Maybe.withDefault "you" name
    in
    "One for " ++ who ++ ", one for me."
