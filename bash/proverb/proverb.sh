#!/usr/bin/env bash

for (( i=1, j=2 ; i < $# ; i++, j++ )); do
    echo "For want of a ${!i} the ${!j} was lost."
done

[[ -n $1 ]] && echo "And all for the want of a $1." || :
