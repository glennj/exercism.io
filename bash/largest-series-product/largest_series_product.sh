#!/usr/bin/env bash

source ./utils.bash 

series=$1
declare -i span=$2

[[ $series =~ [^0-9] ]] && die "input must only contain digits"
((span <= ${#series}))  || die "span must not exceed string length"
((span >= 0))           || die "span must not be negative"

if ((span == 0)); then
    max=1
else
    for ((i = 0; i <= ${#series} - span; i++)); do
        product=1
        for ((j = 0; j < span; j++)); do
            ((product *= ${series:i+j:1}))
        done
        max=$((product > max ? product : max))
    done
fi

echo "$max"
