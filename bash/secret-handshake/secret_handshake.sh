#!/usr/bin/env bash

source ../lib/utils.bash
checkBashVersion 4.3 namerefs

source ../lib/utils_array.bash
source ../lib/utils_string.bash


main() {
    local code=$1
    local actions=( "wink" "double blink" "close your eyes" "jump" )
    local result=()
    local -i i

    for (( i = 0; i < ${#actions[@]}; i++ )); do
        if (( (code & (1 << i)) != 0 )); then
            result+=( "${actions[i]}" )
        fi
    done

    if (( code >= (1 << ${#actions[@]}) )); then
        array::reverse result
    fi

    str::join , "${result[@]}"
}

main "$@"
