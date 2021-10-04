#!/usr/bin/env bash

# A translation of this lovely recursive javascript solution
# https://exercism.io/tracks/javascript/exercises/say/solutions/515ab00bc90f46b0bde3732d9317a46b

# We can be rather relaxed about quoting
# shellcheck disable=SC2086,SC2046,SC2206

readonly low=(
    zero one two three four five six seven eight nine
    ten eleven twelve thirteen fourteen fifteen
    sixteen seventeen eightteen nineteen
)
readonly high=(
    [20]=twenty [30]=thirty [40]=forty [50]=fifty
    [60]=sixty [70]=seventy [80]=eighty [90]=ninety
)

out_of_range() {
    echo 'input out of range' >&2
    exit 1
}

say() {
    local -i n=$1
    if   ((n < 0));      then out_of_range
    elif ((n < 100));    then say_small $n
    elif ((n < 10**3));  then say_compound $n 100 hundred
    elif ((n < 10**6));  then say_compound $n $((10**3)) thousand
    elif ((n < 10**9));  then say_compound $n $((10**6)) million
    elif ((n < 10**12)); then say_compound $n $((10**9)) billion
    else                      out_of_range
    fi
}

say_small() {
    local -i n=$1
    if ((n < 20)); then
        echo ${low[n]}
    elif [[ -n ${high[n]} ]]; then
        echo ${high[n]}
    else
        printf '%s-%s\n' \
            $(say $((n - (n % 10)))) \
            $(say $((n % 10)))
    fi
}

say_compound() {
    local -i n=$(($1 / $2)) rem=$(($1 % $2))
    # shellcheck disable=SC2207
    local -a saying=($(say $n) $3)
    # shellcheck disable=SC2207
    ((rem > 0)) && saying+=($(say $rem))
    echo "${saying[*]}"
}

say $(($1))     # coerce the parameter into an integer
