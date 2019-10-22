#!/bin/bash

# this is a situation where is really doesn't make sense
# to implement this in bash
#
# external tools used: tr, grep, sort, uniq, awk
#echo "${1//[-[:blank:]]/}" | 
#  tr '[:upper:]' '[:lower:]' |
#  grep -o . |
#  sort |
#  uniq -c |
#  awk '$1 > 1 {exit 1}' && echo true || echo false


# but if I was forced to use bash, this:

if [[ ${BASH_VERSINFO[0]} -lt 4 ]]; then
    echo "bash version 4.0 required" >&2
    exit 2
fi

declare -l lc=${1//[-[:blank:]]/}
declare -A count
for ((i=0; i<${#lc}; i++)); do
    char=${lc:i:1}
    (( ++count[$char] > 1 )) && { echo false; exit; }
done
echo true
