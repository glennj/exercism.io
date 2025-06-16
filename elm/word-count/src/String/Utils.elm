module String.Utils exposing (..)


trimSuffix : String -> String -> String
trimSuffix suffix word =
    if String.endsWith suffix word then
        trimSuffix suffix (String.dropRight (String.length suffix) word)

    else
        word
