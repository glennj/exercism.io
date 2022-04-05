#!/usr/bin/env bash

# There must be at least 2 positional parameters 
# to enter the loop:
#   `i` initialized to 1
#   `i < $#` test passes only if 2 or more parameters

for (( i=1, j=2 ; i < $# ; i++, j++ )); do
    echo "For want of a ${!i} the ${!j} was lost."
done

# And at least one parameter to print this:

[[ -n $1 ]] && echo "And all for the want of a $1." || :
