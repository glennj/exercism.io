module Tests exposing (tests)

import Expect
import RomanNumerals exposing (toRoman)
import Test exposing (..)


tests : Test
tests =
    describe "Roman Numerals"
        [ test "1" <|
            \() ->
                Expect.equal "I"
                    (toRoman 1)
        , {- {- skip <| -} -}
            test "2" <|
                \() ->
                    Expect.equal "II"
                        (toRoman 2)
        , {- {- skip <| -} -}
            test "3" <|
                \() ->
                    Expect.equal "III"
                        (toRoman 3)
        , {- {- skip <| -} -}
            test "4" <|
                \() ->
                    Expect.equal "IV"
                        (toRoman 4)
        , {- {- skip <| -} -}
            test "5" <|
                \() ->
                    Expect.equal "V"
                        (toRoman 5)
        , {- {- skip <| -} -}
            test "6" <|
                \() ->
                    Expect.equal "VI"
                        (toRoman 6)
        , {- {- skip <| -} -}
            test "9" <|
                \() ->
                    Expect.equal "IX"
                        (toRoman 9)
        , {- {- skip <| -} -}
            test "16" <|
                \() ->
                    Expect.equal "XVI"
                        (toRoman 16)
        , {- {- skip <| -} -}
            test "27" <|
                \() ->
                    Expect.equal "XXVII"
                        (toRoman 27)
        , {- {- skip <| -} -}
            test "48" <|
                \() ->
                    Expect.equal "XLVIII"
                        (toRoman 48)
        , {- {- skip <| -} -}
            test "59" <|
                \() ->
                    Expect.equal "LIX"
                        (toRoman 59)
        , {- {- skip <| -} -}
            test "66" <|
                \() ->
                    Expect.equal "LXVI"
                        (toRoman 66)
        , {- {- skip <| -} -}
            test "93" <|
                \() ->
                    Expect.equal "XCIII"
                        (toRoman 93)
        , {- {- skip <| -} -}
            test "141" <|
                \() ->
                    Expect.equal "CXLI"
                        (toRoman 141)
        , {- {- skip <| -} -}
            test "163" <|
                \() ->
                    Expect.equal "CLXIII"
                        (toRoman 163)
        , {- {- skip <| -} -}
            test "166" <|
                \() ->
                    Expect.equal "CLXVI"
                        (toRoman 166)
        , {- {- skip <| -} -}
            test "402" <|
                \() ->
                    Expect.equal "CDII"
                        (toRoman 402)
        , {- {- skip <| -} -}
            test "575" <|
                \() ->
                    Expect.equal "DLXXV"
                        (toRoman 575)
        , {- {- skip <| -} -}
            test "666" <|
                \() ->
                    Expect.equal "DCLXVI"
                        (toRoman 666)
        , {- {- skip <| -} -}
            test "911" <|
                \() ->
                    Expect.equal "CMXI"
                        (toRoman 911)
        , {- {- skip <| -} -}
            test "1024" <|
                \() ->
                    Expect.equal "MXXIV"
                        (toRoman 1024)
        , {- {- skip <| -} -}
            test "1666" <|
                \() ->
                    Expect.equal "MDCLXVI"
                        (toRoman 1666)
        , {- {- skip <| -} -}
            test "3000" <|
                \() ->
                    Expect.equal "MMM"
                        (toRoman 3000)
        , {- {- skip <| -} -}
            test "3001" <|
                \() ->
                    Expect.equal "MMMI"
                        (toRoman 3001)
        , {- {- skip <| -} -}
            test "3888" <|
                \() ->
                    Expect.equal "MMMDCCCLXXXVIII"
                        (toRoman 3888)
        , {- {- skip <| -} -}
            test "3999" <|
                \() ->
                    Expect.equal "MMMCMXCIX"
                        (toRoman 3999)
        ]
