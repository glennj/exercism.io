#!/bin/bash

source ./utils.bash
source ./utils_string.bash

[[ $1 == [A-Z] ]] || die "usage: ${0#*/} upper_case_letter"

# `eval` is required here because the shell performs
# brace expansion _before_ variable expansion
#letters=($(eval "echo {A..$1}"))

# but `declare` is often useful to avoid `eval`
declare -ra "letters=({A..$1})"

# shellcheck disable=SC2154
len=${#letters[@]}
segments=()

for ((i = 0; i < len; i++)); do
    segment="$(str::repeat " " "$i")${letters[i]}$(str::repeat " " $((len - i - 1)))"
    segments[i]="$(str::reverse "${segment#?}")$segment"
    echo "${segments[i]}"
done
for ((i = len - 2; i >= 0; i--)); do
    echo "${segments[i]}"
done
