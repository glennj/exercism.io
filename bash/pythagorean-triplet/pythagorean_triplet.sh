#!/usr/bin/env bash

glennj_brute_force() {
    local -ir perimeter=$1
    local -i a b c

    # A brute force solution.
    # Note that the smallest pythagorean triangle is {3,4,5}
    # We'll use those edges in the loop boundaries
    # to save a little bit of time.

    for ((c = perimeter - 3 - 4; c >= 5; c--)); do
        for ((b = c - 1; b >= 4; b--)); do
            a=$((perimeter - c - b))
            if  ((3 <= a && a < b)) &&
                ((a * a + b * b == c * c))
            then
                echo "$a,$b,$c"
            fi
        done
    done
}

# Showing the cost of the brute force solution, an
# exemplary solution by @alexiszam
# https://exercism.io/tracks/bash/exercises/pythagorean-triplet/solutions/97157571130f4749b0e2fb4948834bca
#
alexiszam() {
    (($1 % 2 != 0)) && return

    a=1

    while true; do
        if (($1 * ($1 - 2 * a) % (2 * ($1 - a)) == 0)); then
            ((b = $1 * ($1 - 2 * a) / (2 * ($1 - a))))
            ((a >= b)) && break
            result+="$a,$b,$(($1 - a - b))\n"
        fi
        ((a++))
    done

    echo -en "$result"
}

main() {
    local benchmark=false
    local func=glennj_brute_force
    while getopts :Bab opt; do
        case $opt in
            B) benchmark=true ;;
            a) func=glennj_brute_force ;;
            b) func=alexiszam ;;
            *) : ;;
        esac
    done
    shift $((OPTIND - 1))

    if $benchmark; then
        hyperfine \
            -n alexiszam "bash $0 -b 840" \
            -n glennj    "bash $0 -a 840"
    else
        "$func" "$@"
    fi
}

main "$@"

# Sample benchmarking output
#
# $ bash pythagorean_triplet.sh -B
# Benchmark #1: alexiszam
#   Time (mean ± σ):      12.0 ms ±   1.2 ms    [User: 5.2 ms, System: 2.7 ms]
#   Range (min … max):     9.0 ms …  17.5 ms    117 runs
#
# Benchmark #2: glennj
#   Time (mean ± σ):      4.584 s ±  0.041 s    [User: 4.562 s, System: 0.011 s]
#   Range (min … max):    4.552 s …  4.695 s    10 runs
#
# Summary
#   'alexiszam' ran
#   383.20 ± 38.07 times faster than 'glennj'
#
