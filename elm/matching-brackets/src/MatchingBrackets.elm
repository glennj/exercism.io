module MatchingBrackets exposing (isPaired)

import Dict exposing (Dict)
import List


isPaired : String -> Bool
isPaired input =
    checkPaired (String.toList input) []


brackets : Dict Char Char
brackets =
    -- map close brackets to their corresponding open brackets
    Dict.fromList
        [ ( ']', '[' )
        , ( '}', '{' )
        , ( ')', '(' )
        ]


checkPaired : List Char -> List Char -> Bool
checkPaired chars stack =
    case chars of
        [] ->
            -- input is paired if no open brackets left on stack.
            List.isEmpty stack

        char :: rest ->
            case Dict.get char brackets of
                Just openBracket ->
                    -- char is a close bracket.
                    -- Does the head of the stack match the open bracket?
                    case stack of
                        [] ->
                            False

                        b :: bs ->
                            if b /= openBracket then
                                False

                            else
                                checkPaired rest bs

                Nothing ->
                    -- char is not a close bracket.
                    -- Is it an open bracket?
                    if List.member char (Dict.values brackets) then
                        -- add it to the stack and continue
                        checkPaired rest (char :: stack)

                    else
                        -- ignore this char and continue
                        checkPaired rest stack
