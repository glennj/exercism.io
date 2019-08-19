#!/usr/bin/env bash

colors=(
    black brown red orange yellow 
    green blue violet grey white
)

main() {
    local result=""
    for color in "${@:1:2}"; do
        result+=$(code "$color") || die "invalid color"
    done
    echo "$result"
}

die() { echo "$*" >&2; exit 1; }

code() {
    local -i code=-1 i
    for i in "${!colors[@]}"; do
        if [[ ${colors[i]} == "$1" ]]; then
            code=$i
            break
        fi
    done
    (( code == -1 )) && return 1
    echo $code
}

main "$@"
