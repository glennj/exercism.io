#!/bin/bash

set -- '' ''
source ./hamming.sh >/dev/null

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

for func in hamming_forLoop hamming_whileReadLoop; do
    printf "\n%s\n\n" "$func"
    time {
        for ((i=0; i<10; i++)); do
            "$func" "$strand1" "$strand2" >/dev/null
        done
    }
done
