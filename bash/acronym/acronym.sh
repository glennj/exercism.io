#!/bin/bash
acronym=""
IFS=" -"                  # space or hyphen separates words
set -f                    # disable path expansion
for word in $1; do        # $1 specifically unquoted
    acronym+=${word:0:1}
done
echo "${acronym^^}"
