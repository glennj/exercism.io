#!/bin/bash
acronym=""
IFS=" -_"                 # characters that separate words

read -ra words <<<"$1"    # split the input into words
for word in "${words[@]}"; do
    acronym+=${word:0:1}
done

# without using an array:
##set -f                    # disable path expansion
##for word in $1; do        # $1 specifically unquoted
##    acronym+=${word:0:1}
##done

echo "${acronym^^}"
