#!/usr/bin/env bash 


source ../lib/utils.bash
source ../lib/utils_string.bash
source ../lib/utils_math.bash
checkBashVersion 4.0 "'local -l'"

shopt -u nocasematch

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
        encode|decode) "$1" "$2" "$key" ;;
        key) echo "$key" ;;
        *)   echo "unknown subcommand: $OPTARG" >&2 ;;
    esac
}

# 100 random lower case letters
generate_key() {
    local key=""
    local -i a=$(str::ord "a") i idx
    for i in {1..100}; do
        idx=$(( RANDOM % 26 ))
        key+=$(str::chr $((a + idx)))
    done
    echo "$key"
}

# the key must contain only lowercase letters
validate_key() {
    local key=$1
    if [[ $key == *[^[:lower:]]* ]]; then
        echo "invalid key: contains non-lc-letter" >&2
        exit 1
    fi
}

encode() { _code "$1" "$2"  1; }

decode() { _code "$1" "$2" -1; }

_code() {
    local -l plaintext=$1 key=$2        # lower case
    local -i direction=$3
    local -i a=$(str::ord "a") i idx p k
    local encoded=""

    # ensure the key is long enough
    while [[ ${#key} -lt ${#plaintext} ]]; do key+=$key; done

    for (( i = 0; i < ${#plaintext}; i++ )); do
        p=$(( $(str::ord "${plaintext:i:1}") - a ))
        k=$(( $(str::ord "${key:i:1}")       - a ))

        # (-5 % 26) == -5, but we want to normalize it
        # to be between 0 and 25
        #idx=$(( (((p + k * direction) % 26) + 26) % 26 ))
        idx=$( math::floorMod $((p + k * direction)) 26 )
        encoded+=$(str::chr $((a + idx)))
    done
    echo "$encoded"
}

main "$@"
