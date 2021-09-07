#!/usr/bin/env bash

source ./utils.bash
checkBashVersion 4.3 namerefs

declare -rA plants=(
    [G]=grass [C]=clover [R]=radishes [V]=violets
)
declare -rA studentIdx=(
    [Alice]=0  [Bob]=1    [Charlie]=2  [David]=3
    [Eve]=4    [Fred]=5   [Ginny]=6    [Harriet]=7
    [Ileana]=8 [Joseph]=9 [Kincaid]=10 [Larry]=11
)

# Given an input diagram like $'ABCDEF\nGHIJKL'
# populate the named array with: ("ABGH" "CDIJ" "EFKL")
parse() {
    local -n __plots=$2
    local row1 row2

    # split the first argument on newlines into row vars
    { read -r row1; read -r row2; } <<< "$1"
    assert "${#row1} == ${#row2} && ${#row1} % 2 == 0" "invalid garden"

    while [[ -n $row1 ]]; do
        __plots+=("${row1:0:2}${row2:0:2}")
        row1=${row1#??}
        row2=${row2#??}
    done
}

# Given a "plant code" string like "VRCG",
# print a  list of plant names: "violets radishes clover grass"
expand() {
    local -a result
    local plot=$1 plant i
    for ((i = 0; i < ${#plot}; i++)); do
        plant=${plot:i:1}
        assert -C [ -v "plants[$plant]" ] "unknown plant: $plant"
        result+=("${plants[$plant]}")
    done
    echo "${result[*]}"
}

main() {
    assert -C [ -v "studentIdx[$2]" ] "unknown student: $2"
    local -a plots
    parse "$1" plots
    expand "${plots[${studentIdx[$2]}]}"
}

main "$@"
