module PhoneNumber exposing (getNumber)

import Regex


getNumber : String -> Maybe String
getNumber phoneNumber =
    let valid = Regex.fromString "^1?([2-9]\\d\\d[2-9]\\d\\d\\d{4})$"
                    |> Maybe.withDefault Regex.never

        matches = phoneNumber
                    |> String.filter Char.isDigit
                    |> Regex.find valid
    in
    case matches of
        [ m ] ->
            case m.submatches of
                [ num ] -> num
                _ -> Nothing
        _ -> Nothing
