#!/usr/bin/env bash

if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then
    echo "This library of functions should be sourced into another script" >&2
    exit 4
fi

source ./utils_math.bash   # `math::add`

# using function "namespaces" to differentiate implemenations

# for benchmarking the different implemenations, please see
# https://github.com/glennj/exercism.io/tree/main/bash/grains

############################################################
# implementation calling out to `bc`

bc::atSquare() { echo "2 ^ ($1 - 1)" | bc; }

bc::total() {
    # this implementation, while nice and DRY,
    # is calling bc 65 times:
    #for i in {1..64}; do
    #    pow2 $i
    #done | paste -sd + | bc

    expression="0"
    for i in {1..64}; do
        expression+=" + (2^($i-1))"
    done
    echo "$expression" | bc
}

############################################################
# A version with no external tools used.
#
# I do not recommend this approach if you care about performance.

# shellcheck disable=SC2086

bash_only::calcSquares() {
    local OPTIND calcTotal=false
    while getopts "T" opt; do
        [[ $opt == T ]] && calcTotal=true
    done
    shift $((OPTIND - 1))

    square=(0 1)
    $calcTotal && total=${square[1]}

    for ((i = 2; i <= $1; i++)); do
        square[i]=$(math::add ${square[i - 1]} ${square[i - 1]})
        $calcTotal && total=$(math::add $total ${square[i]})
    done
}

bash_only::atSquare() {
    local -a square
    bash_only::calcSquares "$1"
    echo ${square[$1]}
}

bash_only::total() {
    local total
    local -a square
    bash_only::calcSquares -T 64
    echo $total
}

############################################################
# with the values merely hardcoded

precalculated::total() { echo 18446744073709551615; }
precalculated::atSquare() {
    case $1 in
         1) echo 1 ;;
         2) echo 2 ;;
         3) echo 4 ;;
         4) echo 8 ;;
         5) echo 16 ;;
         6) echo 32 ;;
         7) echo 64 ;;
         8) echo 128 ;;
         9) echo 256 ;;
        10) echo 512 ;;
        11) echo 1024 ;;
        12) echo 2048 ;;
        13) echo 4096 ;;
        14) echo 8192 ;;
        15) echo 16384 ;;
        16) echo 32768 ;;
        17) echo 65536 ;;
        18) echo 131072 ;;
        19) echo 262144 ;;
        20) echo 524288 ;;
        21) echo 1048576 ;;
        22) echo 2097152 ;;
        23) echo 4194304 ;;
        24) echo 8388608 ;;
        25) echo 16777216 ;;
        26) echo 33554432 ;;
        27) echo 67108864 ;;
        28) echo 134217728 ;;
        29) echo 268435456 ;;
        30) echo 536870912 ;;
        31) echo 1073741824 ;;
        32) echo 2147483648 ;;
        33) echo 4294967296 ;;
        34) echo 8589934592 ;;
        35) echo 17179869184 ;;
        36) echo 34359738368 ;;
        37) echo 68719476736 ;;
        38) echo 137438953472 ;;
        39) echo 274877906944 ;;
        40) echo 549755813888 ;;
        41) echo 1099511627776 ;;
        42) echo 2199023255552 ;;
        43) echo 4398046511104 ;;
        44) echo 8796093022208 ;;
        45) echo 17592186044416 ;;
        46) echo 35184372088832 ;;
        47) echo 70368744177664 ;;
        48) echo 140737488355328 ;;
        49) echo 281474976710656 ;;
        50) echo 562949953421312 ;;
        51) echo 1125899906842624 ;;
        52) echo 2251799813685248 ;;
        53) echo 4503599627370496 ;;
        54) echo 9007199254740992 ;;
        55) echo 18014398509481984 ;;
        56) echo 36028797018963968 ;;
        57) echo 72057594037927936 ;;
        58) echo 144115188075855872 ;;
        59) echo 288230376151711744 ;;
        60) echo 576460752303423488 ;;
        61) echo 1152921504606846976 ;;
        62) echo 2305843009213693952 ;;
        63) echo 4611686018427387904 ;;
        64) echo 9223372036854775808 ;;
    esac
}
