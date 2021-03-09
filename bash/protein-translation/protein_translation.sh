#!/usr/bin/env bash

source ../lib/utils.bash
checkBashVersion 4.0 "associative arrays"

declare -A map=(
    [UAA]="STOP"          [UAG]="STOP"        [UGA]="STOP"
    [UUU]="Phenylalanine" [UUC]="Phenylalanine"
    [UUA]="Leucine"       [UUG]="Leucine"
    [UCU]="Serine"        [UCC]="Serine"
    [UCA]="Serine"        [UCG]="Serine"
    [UAU]="Tyrosine"      [UAC]="Tyrosine"
    [UGU]="Cysteine"      [UGC]="Cysteine"
    [UGG]="Tryptophan"
    [AUG]="Methionine"
)

proteins=()

codons=$1

while [[ $codons =~ (...)(.*) ]]; do
    codon=${BASH_REMATCH[1]}
    codons=${BASH_REMATCH[2]}
    protein=${map[$codon]}
    [[ -n $protein ]] || die "Invalid codon"
    [[ $protein == STOP ]] && break
    proteins+=("$protein")
done

echo "${proteins[*]}"
