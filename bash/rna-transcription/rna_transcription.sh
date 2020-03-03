#!/bin/bash

source ../lib/utils.bash
checkBashVersion 4.0 "associative arrays"

if [[ $1 == *[^GCTA]* ]]; then
    echo "Invalid nucleotide detected." >&2
    exit 1
fi

declare -A map=( [G]=C [C]=G [T]=A [A]=U )
rna=""
for (( i = 0; i < ${#1}; i++ )); do
    rna+=${map[${1:i:1}]}
done
echo "$rna"
