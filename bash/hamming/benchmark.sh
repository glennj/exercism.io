#!/usr/bin/env bash

readonly alpha=({a..z})

makeString() {
    local -i len=$1
    local str
    echo "creating a string of $len random letters"
    for ((i = 0; i < len; i++)); do
        str+=${alpha[RANDOM % 26]}
    done
    echo "$str"
}


dotest() {
    "$1" "$strand1" "$strand2"
}

set -- '' '' # the main loop in hamming.sh requires 2 positional params
source ./hamming.sh >/dev/null

benches=(
    -n whileLoop 'bash -c "dotest hamming_whileReadLoop"'
    -n forLoop   'bash -c "dotest hamming_forLoop"'
)

export -f hamming_forLoop hamming_whileReadLoop
export -f dotest
export strand1 strand2

for len in 127 255 4095 32767; do
    strand1=$(makeString $len)
    strand2=${strand1%?}X       # only the last char is different

    echo "benchmark with length $len"
    echo
    hyperfine "${benches[@]}"
    echo
done
