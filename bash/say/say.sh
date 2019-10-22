#!/usr/bin/env bash

# translation of this lovely recursive javascript solution
# https://exercism.io/tracks/javascript/exercises/say/solutions/515ab00bc90f46b0bde3732d9317a46b

# `-v` test operator introduced in bash 4.3
if    [[ ${BASH_VERSINFO[0]} -lt 4 ]] ||
    { [[ ${BASH_VERSINFO[0]} -eq 4 ]] && [[ ${BASH_VERSINFO[1]} -lt 3 ]]; }
then
    echo "bash version 4.3 required" >&2
    exit 2
fi

low=(
    zero one two three four five six seven eight nine
    ten eleven twelve thirteen fourteen fifteen
    sixteen seventeen eightteen nineteen
)
high=(
    [20]=twenty [30]=thirty  [40]=forty  [50]=fifty
    [60]=sixty  [70]=seventy [80]=eighty [90]=ninety
)

say() {
    local -i n=$1
    if (( n < 0 )); then 
        echo "input out of range" >&2
        exit 1
    elif (( n < 100 )); then
        say_small $n
    elif (( n < 1000 )); then
        say_compound $n 100 hundred
    elif (( n < 1000000 )); then
        say_compound $n 1000 thousand
    elif (( n < 1000000000 )); then
        say_compound $n 1000000 million
    elif (( n < 1000000000000 )); then
        say_compound $n 1000000000 billion
    else
        echo "input out of range" >&2
        exit 1
    fi
}

say_small() {
    local -i n=$1
    if (( n < 20 )); then
        echo "${low[n]}"
    elif [[ -v high[$n] ]]; then
        echo "${high[$n]}"
    else
        printf "%s-%s\n" $(say $((n - (n%10)))) $(say $((n%10)))
    fi
}

say_compound() {
    local -i n=$(( $1 / $2 )) rem=$(( $1 % $2 ))
    local -a saying=( $(say $n) $3 )
    (( rem > 0 )) && saying+=( $(say $rem) )
    echo "${saying[*]}"
}

say "$1"
