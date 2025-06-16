module WordCount exposing (wordCount)

import Dict exposing (Dict)
import Dict.Utils
import String.Utils


type State
    = SeekingStart
    | AccumulatingWord


wordCount : String -> Dict String Int
wordCount sentence =
    sentence
        |> getWords SeekingStart [] []
        |> List.foldl Dict.Utils.incr Dict.empty


getWords : State -> List Char -> List String -> String -> List String
getWords state wordChars words sentence =
    case ( state, String.uncons sentence ) of
        ( SeekingStart, Nothing ) ->
            words

        ( AccumulatingWord, Nothing ) ->
            toWord wordChars :: words

        ( SeekingStart, Just ( char, rest ) ) ->
            if Char.isAlpha char || Char.isDigit char then
                getWords AccumulatingWord [ char ] words rest

            else
                getWords SeekingStart [] words rest

        ( AccumulatingWord, Just ( char, rest ) ) ->
            if Char.isAlpha char || Char.isDigit char || char == '\'' then
                getWords AccumulatingWord (char :: wordChars) words rest

            else
                getWords SeekingStart [] (toWord wordChars :: words) rest


toWord : List Char -> String
toWord wordChars =
    wordChars
        |> List.reverse
        |> String.fromList
        |> String.toLower
        |> String.Utils.trimSuffix "'"
