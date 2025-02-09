module Acronym exposing (abbreviate)


abbreviate : String -> String
abbreviate phrase =
    doAbbr "start" "" (String.toUpper phrase)


doAbbr : String -> String -> String -> String
doAbbr state acronym phrase =
    case String.uncons phrase of
        Nothing ->
            String.reverse acronym

        Just (char, rest) ->
            -- seeking the first letter at the start of a word
            if state == "start" && Char.isAlpha char then
                doAbbr "end" (String.cons char acronym) rest

            -- seeking the non-word char at the end of a word
            else if state == "end" && not (isWordChar char) then
                doAbbr "start" acronym rest

            else
                doAbbr state acronym rest


isWordChar : Char -> Bool
isWordChar char = Char.isAlpha char || char == '\''
