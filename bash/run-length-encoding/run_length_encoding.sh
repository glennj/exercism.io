#!/usr/bin/env bash

source ./utils.bash
source ./utils_string.bash

encode() {
    local phrase=$1
    [[ -z $phrase ]] && return
    [[ $phrase == *[[:digit:]]* ]] &&
        die "Cannot decode the encoding of a phrase with digits"

    local result="" count=0 char=${phrase:0:1}
    for ((i = 0; i < ${#phrase}; i++)); do
        if [[ ${phrase:i:1} == "$char" ]]; then
            ((count++))
        else
            result+="$(encode_sequence "$count" "$char")"
            char=${phrase:i:1}
            count=1
        fi
    done
    result+="$(encode_sequence "$count" "$char")"
    echo "$result"
}

encode_sequence() {
    local count=$1 char=$2
    ((count == 1)) && count=""
    echo "${count}${char}"
}

decode() {
    local phrase=$1
    local result=""
    local count char

    while [[ $phrase =~ ([[:digit:]]+)([^[:digit:]]) ]]; do
        printf -v phrase "%s%s%s" \
            "${phrase%%${BASH_REMATCH[0]}*}" \
            "$(str::repeat "${BASH_REMATCH[2]}" "${BASH_REMATCH[1]}")" \
            "${phrase#*${BASH_REMATCH[0]}}"
    done
    echo "$phrase"
}

main() {
    case $1 in
        encode | decode) "$1" "$2" ;;
        *) die "unknown subcommand" ;;
    esac
}

main "$@"
