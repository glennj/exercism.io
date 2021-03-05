#!/bin/bash

source ../lib/utils.bash    # `assert`

assert "$# == 2"        "Usage: ${0#*/} <string1> <string2>"
assert "${#1} == ${#2}" "left and right strands must be of equal length"

# Iterate over the length of the string, extracting
# the character at the specified offset
hamming_forLoop() {
    local dist=0 i
    for ((i = 0; i < ${#1}; i++)); do
        [[ ${1:i:1} == "${2:i:1}" ]] || ((dist++))
    done
    echo "$dist"
}

# It's faster to read a string char-by-char, although
# the syntax is more complex.
#
# This seems to fail about 30% of the time when using bash 
# versions < 4.3 with:
# >> hamming.sh: line 21: /dev/fd/61: Interrupted system call
# I wonder if it's due to file descriptor handling in the
# bash version, or some race condition on my Mac, or something else.
#
hamming_whileReadLoop() {
    local dist=0 a b
    while IFS= read -r -n1 a <&3 &&
          IFS= read -r -n1 b <&4
    do
        [[ $a == "$b" ]] || ((dist++))
    done \
        3< <(printf "%s" "$1") \
        4< <(printf "%s" "$2")
    echo $dist
}

#hamming_forLoop "$@"
hamming_whileReadLoop "$@"
