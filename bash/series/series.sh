#!/usr/bin/env bash

source ./utils.bash

series=$1
declare -i slice=$2          # non-numeric values result in 0

assert "slice != 0"          "slice length cannot be zero"
assert "slice > 0"           "slice length cannot be negative"
assert "${#series} > 0"      "series cannot be empty"
assert "${#series} >= slice" "slice length cannot be greater than series length"

sequences=()
for ((i = 0; i <= ${#series} - slice; i++)); do
    sequences+=("${series:i:slice}")
done
echo "${sequences[*]}"
