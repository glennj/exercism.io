#!/bin/bash

source ../lib/utils.bash
checkBashVersion 4.3 "-v operator"

declare -A count=( [A]=0 [C]=0 [G]=0 [T]=0 )

# read the string one character at a time
while IFS= read -d "" -r -n1 c; do 
    [[ -v count[$c] ]] || die "Invalid nucleotide in strand"
    (( count[$c] += 1 ))
done < <(printf "%s" "$1")

# that printf process substitution has an advantage over the
# here-string `<<< "$1"` -- the here-string appends a newline.

# need to specify the output order: "${!count[@]}" is unordered.
for n in A C G T; do
    echo "$n: ${count[$n]}"
done
