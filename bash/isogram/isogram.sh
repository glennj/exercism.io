#!/usr/bin/env bash

# Set a variable for each letter seen.  If the variable
# already exists, the letter has occurred before so the
# input cannot be an isogram.

# Ensure we're not polluted by environment variables
unset {A..Z}

source ../lib/utils.bash
checkBashVersion 4.0 "case conversion"

declare -u char     # upper case

while IFS= read -r -n1 char; do
    if [[ -n ${!char} ]]; then
        echo false
        exit
    fi
    declare "${char}=seen"
done < <(printf '%s' "${1//[^[:alpha:]]/}")

echo true
