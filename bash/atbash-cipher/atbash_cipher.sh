#!/bin/bash

# the alphabet: a bit convoluted, and hardcoding it would
# be simple, but let's demonstrate a dynamic method.
alpha=$( set -- {a..z}; IFS=""; echo "$*" )

declare -A map
for (( i=0, j=${#alpha}-1; i < ${#alpha}; i++, j-- )); do
    map[${alpha:i:1}]=${alpha:j:1}
done


main() {
    if (( $# != 2 )); then
        echo "usage: $(basename "$0") <encode|decode> string" >&2
        exit 1
    fi

    case "$1" in
        encode|decode) "$1" "$2" ;;
        *) echo "error: unknown subcommand '$1'" >&2; exit 1 ;;
    esac
}

encode() { x_code "$1" | groups; }
decode() { x_code "$1"; }
x_code() { echo "$1" | lower_case | alnum_only | encipher; }

# These are nice one line functions that I would use in
# a production script. Let's try them with plain bash.

## lower_case() { tr '[:upper:]' '[:lower:]'; }
## alnum_only() { tr -dc "$alpha[:digit:]"; }
## encipher()   { tr "$alpha" "$(rev <<< "$alpha")"; }
## groups()     { sed -Ee "s/.{${1:-5}}/& /g" -e 's/ $//'; }

lower_case() {
    while IFS= read -r; do
        echo "${REPLY,,}"
    done
}

alnum_only() {
    while IFS= read -r; do
        echo "${REPLY//[^[:alnum:]]/}"
    done
}

encipher() {
    local result char
    while IFS= read -r; do
        result="" 
        for (( i=0; i < ${#REPLY}; i++ )); do
            char=${REPLY:i:1}
            result+=${map[$char]:-$char}
        done
        echo "$result" 
    done
}

groups() {
    local -i size=${1:-5} i
    local -a groups
    while IFS= read -r; do
        groups=()
        for (( i=0; i < ${#REPLY}; i += size )); do
            groups+=( "${REPLY:i:size}" )
        done
        echo "${groups[*]}"
    done
}

main "$@"
