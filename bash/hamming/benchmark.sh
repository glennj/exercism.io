#!/bin/bash

set -- '' '' # the main loop in hamming.sh requires 2 params
source ./hamming.sh >/dev/null
export -f hamming_forLoop hamming_whileReadLoop

alpha=( {a..z} )

# a long string of random letters
for ((i=0; i < 10000; i++)); do
    strand1+=${alpha[ RANDOM % 26 ]}
done

# only the last char is different
strand2=${strand1%?}X

# quick validation
assert "${#strand1} == ${#strand2}" "why are the lengths different"
assert "$(hamming_forLoop $strand1 $strand2) == 1" "distance is 1"
assert "$(hamming_whileReadLoop $strand1 $strand2) == 1" "distance is 1"

hyperfine --warmup 10 --min-runs 200 \
    'hamming_forLoop       "$strand1" "$strand2"' \
    'hamming_whileReadLoop "$strand1" "$strand2"'
