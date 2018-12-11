#!/bin/bash

declare -A letters                      # associative array
declare -l sentence=$1                  # lower case
sentence=${sentence//[^a-z]/}           # remove non-alpha

for (( i = 0; i < ${#sentence}; i++ )); do
    letters[${sentence:i:1}]=1
done
(( ${#letters[@]} == 26 )) && echo true || echo false
