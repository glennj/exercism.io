#!/bin/bash
declare -i sum=0 len=${#1}
for ((i=0; i<len; i++)); do
    (( sum += ${1:i:1} ** len ))
done
((sum == $1)) && echo true || echo false
