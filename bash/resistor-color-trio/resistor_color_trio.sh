#!/usr/bin/env bash
#
# I'm taking some liberties with quoting since I know
# what the values are.

# set up the error messaging with a trap
trap 'echo "unknown color: $color" >&2; exit 1' USR1

# global vars
declare -rA COLORS=(
    [black]=0  [brown]=1  [red]=2     [orange]=3  [yellow]=4
    [green]=5  [blue]=6   [violet]=7  [grey]=8    [white]=9
)
declare -ra PREFIXES=("" kilo mega giga)

valueof() {
    # Throw the signal if it's an invalid color.
    # $$ is the pid of the main shell, even in a subshell.
    [[ -v COLORS[$1] ]] || kill -s USR1 $$
    echo ${COLORS[$1]}
}

resistorValue() {
    echo $(( (10 * $1 + $2) * 10**$3 ))
}

withUnits() {
    local value=$1 unit=$2
    local -i idx=0
    while [[ $value == *000 ]]; do
        value=${value%000}
        ((++idx))
    done
    printf "%d %s%s" $value ${PREFIXES[idx]} "$unit"
}

main() {
    local color v=()
    for color; do
        v+=( $(valueof "$color") )
    done
    withUnits $(resistorValue "${v[@]}") ohms
}

main "$@"
