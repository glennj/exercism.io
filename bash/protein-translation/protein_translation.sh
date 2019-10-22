#!/usr/bin/env bash

if [[ ${BASH_VERSINFO[0]} -lt 4 ]]; then
    echo "bash version 4.0 required" >&2
    exit 2
fi

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

while [[ -n $codons ]]; do
    codon=${codons:0:3}
    protein=${map[$codon]}
    if [[ -z $protein ]]; then
        echo "Invalid codon" >&2
        exit 1
    fi
    [[ $protein == STOP ]] && break
    proteins+=($protein)
    codons=${codons:3}
done

echo "${proteins[*]}"
