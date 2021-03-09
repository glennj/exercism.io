#!/bin/bash

# shellcheck disable=SC2034,SC2219

source ../lib/utils.bash

# We're relying on the (non-)existance of 1-character
# variables. Don't let any existing environment variables
# pollute this script.
unset {A..Z}
declare A=0 C=0 G=0 T=0

while IFS= read -r -n1 c; do
    assert -C [ -n "${!c}" ] "Invalid nucleotide in strand"
    let "$c++"
done < <(printf "%s" "$1")

for n in A C G T; do
    echo "$n: ${!n}"
done
