public function proteins(sequence strand)
    sequence result = {}
    while length(strand) > 0 do
        if length(strand) < 3 then
            return {}
        end if
        sequence protein = get_protein(strand[1..3])
        switch protein do
            case "ERR"  then return {}
            case "STOP" then exit
            case else
                result = append(result, protein)
                strand = strand[4..$]
        end switch
    end while
    return result
end function

function get_protein(sequence nucleotide)
    switch nucleotide do
        case "AUG"                   then return "Methionine"
        case "UUU","UUC"             then return "Phenylalanine"
        case "UUA","UUG"             then return "Leucine"
        case "UCU","UCC","UCA","UCG" then return "Serine"
        case "UAU","UAC"             then return "Tyrosine"
        case "UGU","UGC"             then return "Cysteine"
        case "UGG"                   then return "Tryptophan"
        case "UAA","UAG","UGA"       then return "STOP"
        case else                         return "ERR"
    end switch
end function
