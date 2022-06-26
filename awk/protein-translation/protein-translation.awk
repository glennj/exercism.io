#!/usr/bin/env gawk -f

@include "join"

BEGIN {
    map["AUG"] = "Methionine"
    map["UUU"] = map["UUC"] = "Phenylalanine"
    map["UUA"] = map["UUG"] = "Leucine"
    map["UCU"] = map["UCC"] = map["UCA"] = map["UCG"] = "Serine"
    map["UAU"] = map["UAC"] = "Tyrosine"
    map["UGU"] = map["UGC"] = "Cysteine"
    map["UGG"] = "Tryptophan"
    map["UAA"] = map["UAG"] = map["UGA"] = "STOP"

    FPAT = ".{1,3}"   # relying on this being greedy
}

{
    delete proteins
    n = 0
    for (i = 1; i <= NF; i++) {
        if (!($i in map)) die("Invalid codon")
        protein = map[$i]
        if (protein == "STOP") break
        proteins[++n] = protein
    }
    print join(proteins, 1, n)
}

function die(msg) {
    print msg > "/dev/stderr"
    exit 1
}
