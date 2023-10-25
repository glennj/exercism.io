#!/usr/bin/env gawk -f
@include "arrays"
@include "join"
BEGIN {FS = ""}
function transcribe(nucleotide) {
    switch (nucleotide) {
        case "C": return "G"
        case "G": return "C"
        case "T": return "A"
        case "A": return "U"
        default:
            print "Invalid nucleotide detected." > "/dev/stderr"
            exit 1
    }
}
{
    n = split($0, dna)
    arrays::init(rna)
    arrays::map(dna, "transcribe", rna)
    print join(rna, 1, n, SUBSEP)
    # SUBSEP is a sentinel value: join with empty separator
}
