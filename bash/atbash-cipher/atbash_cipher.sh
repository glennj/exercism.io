#!/bin/bash

source ../lib/utils.bash
checkBashVersion 4.0 "associative arrays"

############################################################
encode() { x_code "$1" | grouped; }
decode() { x_code "$1"; }
x_code() { alnum_only <<<"$1" | lower_case | encipher; }

############################################################
# These are nice one line functions that I would use in
# a production script.

#    lower_case() { tr '[:upper:]' '[:lower:]'; }
#    alnum_only() { tr -dc '[:alnum:]'; }
#    encipher()   { tr '[:lower:]' 'zyxwvutsrqponmlkjihgfedcba'; }
#    groups()     { sed -Ee "s/.{${1:-5}}/& /g" -e 's/ $//'; }

# Since I'm emulating external utilities in plain bash,
# allow these functions to iterate over all the lines that
# are fed to them.

# The default varname for `read` is `$REPLY`

lower_case() {
    while IFS= read -r; do
        printf '%s\n' "${REPLY,,}"
    done
}

alnum_only() {
    while IFS= read -r; do
        printf '%s\n' "${REPLY//[^[:alnum:]]/}"
    done
}

encipher() {
    local result char
    local -A map
    createMap

    while IFS= read -r; do
        result=""

        # a while-read loop to access the characters
        # of a string is faster than a for loop.
        while IFS= read -r -d "" -n1 -u3 char; do
            result+=${map[$char]:-$char}
        done 3<<< "$REPLY"

        printf '%s\n' "$result"
    done
}

createMap() {
    # Create the encryption mapping.
    # I normally avoid external tools in this track,
    # but this is rather pretty:
    local a b
    while read -r a b; do map[$a]=$b; done < <(
        paste <(printf '%s\n' {a..z}) \
              <(printf '%s\n' {z..a})
    )
    
    # in plain bash:
    #    local -a letters=({a..z})
    #    local -i len=${#letters[@]} i
    #    for (( i=0; i<len; i++ )); do
    #        map[${letters[i]}]=${letters[len - i - 1]}
    #    done
}

# This function takes an _optional_ argument:
# shellcheck disable=SC2120
grouped() {
    local -i size=${1:-5} len i
    local grouped
    while IFS= read -r; do
        len=${#REPLY}
        grouped=${REPLY:0:size}
        for (( i=size; i < len; i += size )); do
            grouped+=" ${REPLY:i:size}"
        done
        printf '%s\n' "$grouped"
    done
}

############################################################
main() {
    assert "$# == 2" "usage: ${0##*/} <encode|decode> string"

    case "$1" in
        encode|decode) "$1" "$2" ;;
        *) die "error: unknown subcommand '$1'" ;;
    esac
}

main "$@"
