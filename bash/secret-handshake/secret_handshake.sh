#!/usr/bin/env bash

source ./utils.bash
checkBashVersion 4.3 namerefs
source ./utils_array.bash

readonly actions=("wink" "double blink" "close your eyes" "jump")

main() {
    local code=$1
    local result=()
    local -i i

    for ((i = 0; i < ${#actions[@]}; i++)); do
        if (((code & (1 << i)) != 0)); then
            result+=("${actions[i]}")
        fi
    done

    if ((code >= (1 << ${#actions[@]}))); then
        array::reverse result
    fi

    array::join result ,
}

main "$@"
