module SqueakyClean exposing (clean, clean1, clean2, clean3, clean4)

import List



{- Replace all spaces with underscores -}


clean1 : String -> String
clean1 =
    String.replace " " "_"



{- clean1, plus Replace some control chars with "[CTRL]" -}


clean2 : String -> String
clean2 =
    clean1
        >> (let
                ctrl =
                    [ '\n', '\t', '\u{000D}' ]
            in
            String.foldr
                (\c s ->
                    if List.member c ctrl then
                        "[CTRL]" ++ s

                    else
                        String.cons c s
                )
                ""
           )



{- clean2, plus translate kebab-case to camelCase -}


clean3 : String -> String
clean3 =
    clean2
        >> String.foldl
            (\c ( followsHyphen, s ) ->
                if c == '-' then
                    ( True, s )

                else if followsHyphen then
                    ( False, s ++ String.fromChar (Char.toUpper c) )

                else
                    ( False, s ++ String.fromChar c )
            )
            ( False, "" )
        >> Tuple.second



{- clean3, plus Remove digits -}


clean4 : String -> String
clean4 =
    clean3
        >> String.filter (Char.isDigit >> not)



{- clean4, plus remove lowercase Greek letters -}


clean : String -> String
clean =
    clean4
        >> String.filter (\c -> c < 'α' || c > 'ω')
