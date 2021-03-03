#!/usr/bin/env bash

source ../lib/utils.bash
source ../lib/utils_math.bash
checkBashVersion 4.3 "namerefs"

readonly Alphabet=( {a..z} )

generateMapping() {
    local -n _map=$1
    local -i rotation=$2
    rotation=$( math::floorMod $rotation 26 )

    local rotated=(
        "${Alphabet[@]:rotation}" 
        "${Alphabet[@]:0:rotation}" 
    )

    for i in {0..25}; do
        _map[${Alphabet[i]}]=${rotated[i]}      # lower case
        _map[${Alphabet[i]^}]=${rotated[i]^}    # upper case
    done
}

rotate() {
    local phrase=$1
    local -i rotation=$2
    local result

    local -A mapping
    generateMapping mapping $rotation

    while IFS= read -r -n1 char; do
        result+=${mapping[$char]:-$char}
        # ......................^^^^^^^
        # handles unmapped characters (space, punct, digit)
    done < <(printf '%s' "$phrase")

    echo "$result"
}

rotate "$@"
