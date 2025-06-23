module SimpleCipher exposing (decode, encode, keyGen)

import Random exposing (Generator)



{- Magic numbers:
   97 = Character code of 'a'
   122 = Character code of 'z'
-}


encode : String -> String -> String
encode key plaintext =
    encipher (encipherChar 1) plaintext (keyOffsets plaintext key) ""


decode : String -> String -> String
decode key ciphertext =
    encipher (encipherChar -1) ciphertext (keyOffsets ciphertext key) ""


encipher f text offsets encoded =
    case String.uncons text of
        Nothing ->
            encoded |> String.reverse

        Just ( c, cs ) ->
            case offsets of
                o :: os ->
                    encipher f cs os (String.cons (f c o) encoded)

                _ ->
                    "should not happen"


charIndex char =
    Char.toCode char - 97


encipherChar direction char offset =
    let
        encodedOffset =
            modBy 26 (charIndex char + offset * direction)
    in
    Char.fromCode (97 + encodedOffset)


keyOffsets : String -> String -> List Int
keyOffsets text key =
    let
        stretchKey k =
            if String.length k >= String.length text then
                k
            else
                stretchKey (k ++ k)
    in
    key |> stretchKey |> String.toList |> List.map charIndex


keyGen : Generator String
keyGen =
    let
        codeGen = Random.int 97 122
        letterGen = Random.map Char.fromCode codeGen
        hundredLetters = Random.list 100 letterGen
    in
    Random.map String.fromList hundredLetters
