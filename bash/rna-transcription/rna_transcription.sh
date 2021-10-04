#!/bin/bash

source ./utils.bash
checkBashVersion 4.3 "-v test"

declare -A map=([G]=C [C]=G [T]=A [A]=U)

while IFS= read -r -n1 char; do
    assert -C [ -v "map[$char]" ] "Invalid nucleotide detected."
    rna+=${map[$char]}
done < <(printf '%s' "$1")

echo "$rna"
