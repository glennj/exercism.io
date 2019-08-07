#!/bin/bash

die() { echo "$*" >&2; exit 1; }

(( $# == 2 ))        || die "Usage: $(basename "$0") <string1> <string2>"
[[ -z $1 && -n $2 ]] && die "left strand must not be empty"
[[ -n $1 && -z $2 ]] && die "right strand must not be empty"
(( ${#1} == ${#2} )) || die "left and right strands must be of equal length"

dist=0
for ((i=0; i<${#1}; i++)); do
    [[ ${1:i:1} == "${2:i:1}" ]] || ((dist++))
done
echo "$dist"
