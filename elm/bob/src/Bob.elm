module Bob exposing (hey)


hey : String -> String
hey remark =
    let
        trimmed = String.trim remark
        isSilent = String.isEmpty trimmed
        isAsking = String.endsWith "?" trimmed
        isYelling = String.any Char.isUpper trimmed
                    && String.all (Char.isLower >> not) trimmed
    in
    case ( isSilent, isYelling, isAsking ) of
        ( False, False, False ) -> "Whatever."
        ( False, False, True ) -> "Sure."
        ( False, True, False ) -> "Whoa, chill out!"
        ( False, True, True ) -> "Calm down, I know what I'm doing!"
        ( True, _, _ ) -> "Fine. Be that way!"
