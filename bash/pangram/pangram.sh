#!/usr/bin/env bash

source ./utils.bash
checkBashVersion "4.0" "case conversion"

# ordered by least-frequent usage:
# https://en.wikipedia.org/wiki/Letter_frequency
readonly Alphabet="ZQXJKVBPYGFWMUCLDRHSNIOATE"

test_set_size() {
    local -A letters

    for ((i = 0; i < ${#sentence}; i++)); do
        letters[${sentence:i:1}]=1
    done

    ((${#letters[@]} == 26)) && echo true || echo false
} 

check_each_letter() {
    local letter
    while IFS= read -r -n1 letter; do
        if [[ ! $sentence =~ $letter ]]; then
            echo false
            return
        fi
    done < <(printf '%s' "$Alphabet")
    echo true
}

main() {
    local -u sentence=$1
    sentence=${sentence//[^${Alphabet}]/}

    if ((${#sentence} < 26)); then
        # cannot possibly contain all letters
        echo false
    else
        #test_set_size
        check_each_letter
    fi
}

main "$@"
