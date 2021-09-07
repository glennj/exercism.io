#!/bin/bash

# external tools: grep, sort, paste

source ./utils.bash
checkBashVersion 4.0 "lower-case conversion"

sorted() {
    echo "$1" | grep -o . | sort | paste -sd ""
}

main() {
    local -a anagrams candidates
    local word key candidate

    word=${1,,}
    key=$(sorted "$word")
    read -ra candidates <<< "$2"

    local -l candLower
    for candidate in "${candidates[@]}"; do
        candLower=$candidate

        [[ $word != "$candLower" ]] \
        && [[ $key == "$(sorted "$candLower")" ]] \
        && anagrams+=("$candidate")
    done

    echo "${anagrams[*]}"
}

main "$@"
