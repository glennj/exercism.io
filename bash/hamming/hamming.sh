#!/bin/bash

source ../lib/utils.bash    # `assert`

assert "$# == 2"        "Usage: $(basename "$0") <string1> <string2>"
assert "${#1} == ${#2}" "left and right strands must be of equal length"

dist=0
for ((i=0; i<${#1}; i++)); do
    [[ ${1:i:1} == "${2:i:1}" ]] || ((dist++))
done
echo "$dist"
