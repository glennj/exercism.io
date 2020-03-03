#!/bin/bash

# this is a situation where is really doesn't make sense
# to implement this in bash
#
# external tools used: tr, grep, sort, uniq, awk
#    echo "$1" | 
#    tr -dc '[:alpha:]' |
#    tr '[:upper:]' '[:lower:]' |
#    grep -o . |
#    sort |
#    uniq -c |
#    awk '$1 > 1 {exit 1}' && echo true || echo false


# but if I was forced to use bash, this:

source ../lib/utils.bash
checkBashVersion 4.0 "associative arrays"

declare -A count
declare -l char     # lower case

for ((i=0; i<${#1}; i++)); do
    char=${1:i:1}
    if [[ $char == [[:alpha:]] ]] && (( ++count[$char] > 1 )); then
        echo false
        exit
    fi
done

echo true
