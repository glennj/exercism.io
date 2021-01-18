#!/bin/bash

source ../lib/utils.bash    # `assert`

assert "$# == 2"        "Usage: $(basename "$0") <string1> <string2>"
assert "${#1} == ${#2}" "left and right strands must be of equal length"


# iterate over the length of the string, extracting
# the character at the specified offset
hamming_forLoop() {
    local dist=0 i
    for ((i=0; i<${#1}; i++)); do
        [[ ${1:i:1} == "${2:i:1}" ]] || ((dist++))
    done
    echo "$dist"
}

# it's faster to read a string char-by-char, although
# the syntax is more complex
hamming_whileReadLoop() {
    local dist=0 char1 char2
    while IFS= read -r -d "" -n 1 -u 3 char1 &&
          IFS= read -r -d "" -n 1 -u 4 char2
    do 
        [[ $char1 == "$char2" ]] || ((dist++))
    done \
        3< <(printf "%s" "$1") \
        4< <(printf "%s" "$2")

    echo "$dist"
}

hamming_whileReadLoop "$@"
