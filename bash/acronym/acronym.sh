#!/bin/bash
acronym=""
IFS=" -_"                 # characters that separate words
set -f                    # disable path expansion
for word in $1; do        # $1 specifically unquoted
    acronym+=${word:0:1}
done
echo "${acronym^^}"
