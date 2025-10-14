#!/usr/bin/env bash

source ./utils.bash
source ./utils_string.bash
source ./utils_math.bash
checkBashVersion 4.0 "case conversion"

shopt -u nocasematch

declare -ri alphabet_len=26
declare -ri asc_a=$(str::ord 'a')

# 100 random lower case letters
generate_key() {
    local key=""
    local -i idx
    for _ in {1..100}; do
        idx=$(math::rand 0 $alphabet_len)
        key+=$(str::chr $((asc_a + idx)))
    done
    echo "$key"
}

# the key must contain only lowercase letters
validate_key() {
    local key=$1
    if [[ $key =~ [^[:lower:]] ]]; then
        echo "invalid key: contains non-lc-letter" >&2
        exit 1
    fi
}

encode() { _code "$1" "$2" +1; }
decode() { _code "$1" "$2" -1; }

_code() {
    local -l plaintext=$1 key=$2        # lower case
    local direction=$3 i idx p k
    local keylen=${#key}
    local encoded=""

    for ((i = 0; i < ${#plaintext}; i++)); do
        p=$(($(str::ord "${plaintext:i:1}") - asc_a))
        k=$(($(str::ord "${key:i % keylen:1}") - asc_a))
        idx=$(math::floorMod $((p + k * direction)) $alphabet_len)
        encoded+=$(str::chr $((asc_a + idx)))
    done
    echo "$encoded"
}

main() {
    local key OPTIND OPTARG

    while getopts :k: opt; do
        case $opt in
            k) key=$OPTARG ;;
            ?) echo "unknown option: -$OPTARG" >&2 ;;
        esac
    done
    shift $((OPTIND - 1))

    [[ -z $key ]] && key=$(generate_key)
    validate_key "$key"

    case $1 in
        encode | decode) "$1" "$2" "$key" ;;
        key) echo "$key" ;;
        *)   echo "unknown subcommand: $OPTARG" >&2 ;;
    esac
}

main "$@"
