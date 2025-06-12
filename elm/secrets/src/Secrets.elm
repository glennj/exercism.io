module Secrets exposing (clearBits, decrypt, flipBits, setBits, shiftBack)

import Bitwise


shiftBack amount value =
    Bitwise.shiftRightZfBy amount value


setBits mask value =
    Bitwise.or mask value


flipBits mask value =
    Bitwise.xor mask value


clearBits mask value =
    Bitwise.and (Bitwise.complement mask) value



-- create an integer with only the specific bit positions turned on.
turnOnBits : List Int -> Int
turnOnBits positions =
    let
        setPos posns n =
            case posns of
                [] ->
                    n

                p :: ps ->
                    Bitwise.shiftLeftBy (p - 1) 1
                        |> Bitwise.or n
                        |> setPos ps
    in
    setPos positions 0


decrypt secret =
    secret
        |> setBits 1996
        |> flipBits 2009
        |> shiftBack 5
        |> clearBits (turnOnBits [ 1, 5 ])
