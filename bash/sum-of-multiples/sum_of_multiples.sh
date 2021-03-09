#!/usr/bin/env bash

limit=$1
shift
multiples=()

for factor; do
    # non-positive input does not contribute any factors
    ((factor > 0)) || continue

    for ((i = factor; i < limit; i += factor)); do
        if ((i % factor == 0)); then
            # storing the multiples as array indices,
            # to act like a set of values
            multiples[$i]=1
        fi
    done
done

# add up the array indices
sum=0
for m in "${!multiples[@]}"; do ((sum += m)); done

echo $sum
