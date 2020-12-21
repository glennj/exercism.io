#!/usr/bin/env bash

is_armstrong() {
    local -i num=$1
    local -i sum=0 len=${#num} i

    for (( i=0; i<len; i++ )); do
        (( sum += ${num:i:1} ** len ))
    done

    (( sum == num ))
}

is_armstrong "$1" && echo true || echo false
exit


# golfed wrt LOC:
for (( len=${#1}, i=0; i<len; i++ )); do 
    (( sum += ${1:i:1} ** len ))
done
(( sum == $1 )) && echo true || echo false



