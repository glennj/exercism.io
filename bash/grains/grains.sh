#!/usr/bin/env bash

source ./grains_lib.bash

# pick an implementation:
#namespace="bc"
namespace="bash_only"

case $1 in
    total) "$namespace"::total "$1" ;;

    [1-9] | [1-5][0-9] | 6[0-4] ) "$namespace"::atSquare "$1" ;;
        
    *)  die "Error: invalid input" ;;
esac
