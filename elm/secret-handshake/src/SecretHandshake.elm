module SecretHandshake exposing (Command(..), commands)

import Bitwise exposing (and, shiftRightBy)


type Command
    = Wink
    | DoubleBlink
    | CloseYourEyes
    | Jump


commands : Int -> List Command
commands number =
    let
        commandList =
            [ Wink, DoubleBlink, CloseYourEyes, Jump ]

        numToCmds num cmds acc =
            case ( num, cmds ) of
                ( 0, _ ) -> acc
                ( _, [] ) -> acc
                ( _, c :: cs ) ->
                    if and 1 num == 1 then
                        numToCmds (shiftRightBy 1 num) cs (c :: acc)
                    else
                        numToCmds (shiftRightBy 1 num) cs acc

        result =
            numToCmds number commandList []

        isReversed =
            (shiftRightBy (List.length commandList) number |> and 1) == 1
    in
    if isReversed then
        result

    else
        List.reverse result
