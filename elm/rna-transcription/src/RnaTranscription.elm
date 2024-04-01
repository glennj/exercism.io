module RnaTranscription exposing (toRNA)


toRNA : String -> Result String String
toRNA dna =
    transcribe dna ""


transcribe : String -> String -> Result String String
transcribe dna rna =
    case String.uncons dna of
        Nothing ->
            Ok (String.reverse rna)

        Just ( nucleotide, rest ) ->
            case complement nucleotide of
                Err e  -> Err e
                Ok nuc -> transcribe rest (String.cons nuc rna)


complement : Char -> Result String Char
complement nucleotide =
    case nucleotide of
        'C' -> Ok 'G'
        'G' -> Ok 'C'
        'T' -> Ok 'A'
        'A' -> Ok 'U'
        _   -> Err "Invalid nucleotide"
