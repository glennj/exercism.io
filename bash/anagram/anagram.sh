#!/bin/bash

if [[ ${BASH_VERSINFO[0]} -lt 4 ]]; then
    echo "bash version 4.0 required" >&2
    exit 2
fi

main() {
    local anagrams=()
    set -f   # disable path expansion
    for candidate in $2; do
        isAnagram "$1" "$candidate" && anagrams+=("$candidate")
    done
    echo "${anagrams[*]}"
}

isAnagram() {
    local -l first=$1 second=$2
    [[ "$first" != "$second" ]] && 
    [[ "$(sorted "$first")" == "$(sorted "$second")" ]]
}

sorted() {
    echo "$1" | grep -o . | sort | paste -sd ""
}

main "$@"
