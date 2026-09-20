#!/usr/bin/env bash

shared_birthday() {
    local -A seen
    local day date
    for date; do
        day=${date:5}
        if ${seen[$day]:-false}; then
            echo true
            return
        fi
        seen[$day]=true
    done
    echo false
}

random_birthdates() {
    local y m d i
    local days=("" 31 28 31 30 31 30 31 31 30 31 30 31)
    for ((i=$1; i > 0; i--)); do
        while true; do
            y=$((1900 + RANDOM % 100))
            is_leap_year $y || break
        done
        m=$((1 + RANDOM % 12))
        d=$((1 + RANDOM % days[m]))
        printf '%d-%02d-%02d\n' $y $m $d        
    done
}

is_leap_year() {
    local y=$1
    ((y % 4 == 0 && (y % 100 != 0 || y % 400 == 0)))
}

estimated_probability_of_share_birthday() {
    # https://en.wikipedia.org/wiki/Birthday_problem#Approximations
    awk -v n="$1" 'BEGIN {
        printf("%.0f\n",  100 * (1 - exp(-n * (n - 1) / 730)))
    }'
}

"$1" "${@:2}"
