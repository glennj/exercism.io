#!/bin/bash

source ../lib/utils_string.bash

if (( $# == 0 )) || [[ $1 != [A-Z] ]]; then
    echo "usage: $(basename "$0") upper_case_letter" >&2
    exit 1
fi

# oooh, evil eval!
letters=( $(eval "echo {A..$1}") )

len=${#letters[@]}
segments=()

for ((i=0; i < len; i++)); do
    segment="$(str::repeat " " $i)${letters[i]}$(str::repeat " " $((len - i - 1)))"
    segments[i]="$(str::reverse "${segment#?}")$segment"
    echo "${segments[i]}"
done
for ((i=len-2; i>=0; i--)); do
    echo "${segments[i]}"
done

