#!/usr/bin/env bash

readonly days=(
    "" first second third fourth fifth sixth
    seventh eighth ninth tenth eleventh twelfth
)

readonly gifts=(
    ""
    "a Partridge in a Pear Tree"
    "two Turtle Doves"
    "three French Hens"
    "four Calling Birds"
    "five Gold Rings"
    "six Geese-a-Laying"
    "seven Swans-a-Swimming"
    "eight Maids-a-Milking"
    "nine Ladies Dancing"
    "ten Lords-a-Leaping"
    "eleven Pipers Piping"
    "twelve Drummers Drumming"
)

verse() {
    local presents=()
    local -i n=$1 i
    local comma="," and=""
    for ((i=n; i >= 1; i--)); do
        ((i == 1)) && { comma="."; ((n > 1)) && and="and "; }
        presents+=( "${and}${gifts[i]}${comma}" )
    done
    printf "On the %s day of Christmas my true love gave to me: %s\n" \
        "${days[n]}" \
        "${presents[*]}"
}

main() {
    local -i i
    for ((i=$1; i<=$2; i++)); do
        verse $i
    done
}

main "$@"
