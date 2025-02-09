module NucleotideCount exposing (nucleotideCounts)


type alias NucleotideCounts =
    { a : Int
    , t : Int
    , c : Int
    , g : Int
    }


nucleotideCounts : String -> Result String NucleotideCounts
nucleotideCounts sequence =
    countNucleotides sequence { a = 0, c = 0, g = 0, t = 0 }


countNucleotides : String -> NucleotideCounts -> Result String NucleotideCounts
countNucleotides seq counts =
    case String.uncons seq of
        Nothing -> Ok counts

        Just ( nucleotide, rest ) ->
            Result.andThen (countNucleotides rest) (updateCount nucleotide counts)

            {-
            case updateCount nucleotide counts of
                Err e -> Err e
                Ok  c -> countNucleotides rest c
            -}


updateCount : Char -> NucleotideCounts -> Result String NucleotideCounts
updateCount nucleotide counts =
    let { a, c, g, t } = counts
    in
    case nucleotide of
        'A' -> Ok { counts | a = a + 1 }
        'C' -> Ok { counts | c = c + 1 }
        'G' -> Ok { counts | g = g + 1 }
        'T' -> Ok { counts | t = t + 1 }
        _   -> Err "Invalid nucleotide in strand"
