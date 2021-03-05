#!/usr/bin/env bash

source ../lib/utils.bash
checkBashVersion 4.0 "associative arrays"
source ../lib/utils_math.bash

declare -A alphaIdx
declare -a alphabet
declare -i m=0
for letter in {a..z}; do
    alphaIdx[$letter]=$((m++))
    alphabet+=("$letter")
done

############################################################
E() { echo $(((a * $1 + b) % m)); }
D() { echo $((mmi * ($1 - b) % m)); }

x_code() {
    local text=$2 encoded="" char filter func
    case $1 in
        encode) func=E; filter=groups ;;
        decode) func=D; filter=identity ;;
    esac
    local -i i n
    for ((i = 0; i < ${#text}; i++)); do
        char=${text:i:1}
        if [[ -v alphaIdx[$char] ]]; then
            n=$($func ${alphaIdx[$char]})
            encoded+=${alphabet[n]}
        else
            encoded+=$char
        fi
    done
    echo "$encoded" | $filter
}

############################################################
# These are nice one line pipeline functions that I would
# use in a production script.
#
# identity()   { cat; }
# groups()     { sed -Ee "s/.{${1:-5}}/& /g" -e 's/ $//'; }
#
# Let's try them with plain bash.
#
# The default variable for `read` is `$REPLY`

identity() {
    local REPLY
    while IFS= read -r || [[ -n $REPLY ]]; do
        echo "$REPLY"
    done
}

groups() {
    local -i size=${1:-5} i
    local -a groups
    local REPLY
    while IFS= read -r || [[ -n $REPLY ]]; do
        groups=()
        for ((i = 0; i < ${#REPLY}; i += size)); do
            groups+=("${REPLY:i:size}")
        done
        echo "${groups[*]}"
    done
}
############################################################

main() {
    (($# == 4)) || die "usage: $0 <encode|decode> a b string"
    case "$1" in
        encode | decode)
            local -i a b mmi
            a=$2
            b=$3
            mmi=$(math::mmi "$a" "$m" 2>/dev/null) || die "a and m must be coprime."

            local -l phrase=${4//[^[:alnum:]]/}

            x_code "$1" "$phrase"
            ;;
        *) die "Error: unknown subcommand '$1'" ;;
    esac
}

main "$@"
