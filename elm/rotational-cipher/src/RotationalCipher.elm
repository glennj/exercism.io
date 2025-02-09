module RotationalCipher exposing (rotate)


rotate : String -> Int -> String
rotate text shiftKey =
    String.map (rotateChar shiftKey) text


alphabetSize = 26
letterA = Char.toCode 'A'
lettera = Char.toCode 'a'

rotateChar : Int -> Char -> Char
rotateChar n char =
    case ( Char.isUpper char, Char.isLower char ) of
        ( True, _ ) -> rotateAlpha letterA n char
        ( _, True ) -> rotateAlpha lettera n char
        _           -> char


rotateAlpha : Int -> Int -> Char -> Char
rotateAlpha base n char =
    let index   = Char.toCode char - base
        rotated = (index + n) |> modBy alphabetSize
    in
    Char.fromCode (rotated + base)
