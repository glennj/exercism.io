module AtbashCipher exposing (decode, encode)


decode : String -> String
decode =
    String.filter Char.isAlphaNum
        >> String.toLower
        >> String.map encodeChar


encode : String -> String
encode =
    decode >> chunked 5



-- ------------------------------


charCode =
    { a = Char.toCode 'a', z = Char.toCode 'z' }


encodeChar : Char -> Char
encodeChar char =
    if Char.isAlpha char then
        Char.fromCode (charCode.z - (Char.toCode char - charCode.a))

    else
        char


chunked : Int -> String -> String
chunked size =
    let
        chunker chunks s =
            if String.isEmpty s then
                List.reverse chunks

            else
                chunker (String.left size s :: chunks) (String.dropLeft size s)
    in
    chunker [] >> String.join " "
