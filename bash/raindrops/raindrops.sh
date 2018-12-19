#!/bin/bash
shopt -s extglob
[[ $# -ne 1 || $1 != +([0-9]) ]] && exit 1

sounds=([3]=Pling [5]=Plang [7]=Plong)
output=""

for i in 3 5 7; do
    (( $1 % i == 0 )) && output+=${sounds[i]}
done

## alternate implementation:
# factors=" $(factor $1 | cut -d: -f2) "   # with the spaces
# for i in 3 5 7; do
#     [[ $factors == *" $i "* ]] && output+=${sounds[i]}
# done

[[ "$output" ]] && echo $output || echo $1
# or
# echo ${output:-$1}
