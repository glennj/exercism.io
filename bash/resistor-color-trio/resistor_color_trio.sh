#!/usr/bin/env bash
#
# I'm taking some liberties with quoting since I know
# what the values are.
#
# shellcheck disable=SC2086,SC2207,SC2046

source ../lib/utils.bash
source ../lib/resistor_color.bash

declare -ra PREFIXES=("" kilo mega giga)

resistorValue() {
    echo $(((10 * $1 + $2) * 10 ** $3))
}

withUnits() {
    local value=$1 unit=$2
    local -i idx=0
    while [[ $value == *000 ]]; do
        value=${value%000}
        ((idx++))
    done
    printf "%d %s%s" $value ${PREFIXES[idx]} "$unit"
}

main() {
    local color values=()
    for color in "${@:1:3}"; do
        values+=($(colorValue "$color")) || die "unknown color: $color"
    done
    withUnits $(resistorValue "${values[@]}") ohms
}

main "$@"
