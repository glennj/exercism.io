#!/bin/bash

if [[ $# -ne 2 ]]; then
    echo "Usage: $(basename "$0") <strand1> <strand2>" >&2
    exit 1
fi

if [[ ${#1} -ne ${#2} ]]; then
    echo "left and right strands must be of equal length" >&2
    exit 1
fi

dist=0
for ((i=0; i<${#1}; i++)); do
    if [[ ${1:i:1} != "${2:i:1}" ]]; then
        ((dist++))
    fi
done
echo "$dist"
