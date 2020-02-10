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
    local code
    for code in "${!colors[@]}"; do
        [[ $1 == ${colors[code]} ]] && echo $code && return
    done
    return 1
}

main "$@"
