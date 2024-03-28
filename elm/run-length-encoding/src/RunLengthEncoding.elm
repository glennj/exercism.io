module RunLengthEncoding exposing (decode, encode)

import Regex


createRegexFromString =
    Regex.fromString >> Maybe.withDefault Regex.never


encode : String -> String
encode =
    Regex.replace
        ("(.)\\1+" |> createRegexFromString)
        (\{ match } -> String.fromInt (String.length match) ++ String.left 1 match)


decode : String -> String
decode =
    Regex.replace
        ("(\\d+)(\\D)" |> createRegexFromString)
        (\{ submatches } ->
            case submatches of
                [ Just digits, Just letter ] ->
                    case String.toInt digits of
                        Just n -> String.repeat n letter
                        _ -> {- should not happen -} ""
                _ -> {- should not happen -} ""
        )



{- Iteration 1, using String.foldl
 ---------------------------------

   decode : String -> String
   decode =
       String.foldl
           (\c ( decoded, num ) ->
               let
                   s = String.fromChar c
               in
               case String.toInt s of
                   Nothing ->
                       ( decoded ++ String.repeat (max num 1) s, 0 )

                   Just digit ->
                       ( decoded, num * 10 + digit )
           )
           ( "", 0 )
           >> Tuple.first


   encode : String -> String
   encode string =
       case String.uncons string of
           Nothing ->
               ""

           Just ( first, rest ) ->
               let
                   appendEncoding s n c =
                       if n == 1 then
                           s ++ String.fromChar c

                       else
                           s ++ String.fromInt n ++ String.fromChar c

                   ( encoded, remaining, char ) =
                       String.foldl
                           (\c ( enc, prevCount, prevChar ) ->
                               if c == prevChar then
                                   ( enc, prevCount + 1, c )

                               else
                                   ( appendEncoding enc prevCount prevChar, 1, c )
                           )
                           ( "", 1, first )
                           rest
               in
               appendEncoding encoded remaining char
-}
