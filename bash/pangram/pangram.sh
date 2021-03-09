#!/usr/bin/env bash

source ../lib/utils.bash
checkBashVersion "4.0" "case conversion"

test_set_size() {
    local -A letters
    local -l sentence=${1//[^[:alpha:]]/}

    for ((i = 0; i < ${#sentence}; i++)); do
        letters[${sentence:i:1}]=1
    done
    ((${#letters[@]} == 26)) && echo true || echo false
}

check_each_letter() {
    local -u sentence=$1

    # ordered by least-frequent usage:
    # https://en.wikipedia.org/wiki/Letter_frequency

    for c in Z Q X J K V B P Y G F W M U C L D R H S N I O A T E; do
        if [[ ! $sentence =~ $c ]]; then
            echo false
            return
        fi
    done
    echo true
}

#test_set_size "$1"
check_each_letter "$1"
