#!/bin/bash
if [[ "$1" == *[^ACGT]* ]]; then
    echo "Invalid nucleotide in strand" >&2
    exit 1
fi

declare -A count=( [A]=0 [C]=0 [G]=0 [T]=0 )

while read n; do (( count[$n]++ )); done < <( grep -o . <<<"$1" )

# or, bash-only
# for ((i=0; i<${#1}; i++)); do (( count[${1:i:1}]++ )); done

for n in A C G T; do
    echo "$n: ${count[$n]}"
done
