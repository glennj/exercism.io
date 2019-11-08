#!/bin/bash

if [[ $# -ne 1 ]] || ! [[ $1 == +([0-9]) && $1 -gt 0 ]]; then
    echo "Error: Only positive numbers are allowed" >&2
    exit 1
fi

n=$1
for (( steps=0; n > 1; n=(n%2 == 1 ? n*3+1 : n/2), steps++ )); do :; done
echo $steps
