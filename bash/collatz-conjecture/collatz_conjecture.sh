#!/usr/bin/env bash

source ./utils.bash
source ./utils_string.bash

if (($# != 1)) || ! str::isInt "$1" || (($1 <= 0)); then
    die "Error: Only positive numbers are allowed"
fi

declare -i n=$1 steps=0

while ((n > 1)); do
    case $((n % 2)) in
        0) ((n /= 2)) ;;
        1) ((n = 3 * n + 1)) ;;
    esac
    ((steps++))
done

echo "$steps"
