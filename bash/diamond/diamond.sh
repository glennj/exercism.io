#!/bin/bash
# external tools used: perl, rev

if [[ ${BASH_VERSINFO[0]} -lt 4 ]]; then
    echo "bash version 4.0 required" >&2
    exit 2
fi

if (( $# == 0 )) || [[ $1 != [A-Z] ]]; then
    echo "usage: $(basename "$0") upper_case_letter" >&2
    exit 1
fi

mapfile -t letters < <(perl -E 'say for ("A" .. shift)' $1)
len=${#letters[@]}
segments=()

# a string containing the requested number of spaces
blanks() { printf "%*s" $1 ""; }

for ((i=0; i < len; i++)); do
    segment="$(blanks $i)${letters[i]}$(blanks $((len - i - 1)))"
    segments[i]="$(echo "${segment#?}" | rev)$segment"
    echo "${segments[i]}"
done
for ((i=len-2; i>=0; i--)); do
    echo "${segments[i]}"
done

