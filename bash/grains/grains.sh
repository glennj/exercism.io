#!/bin/bash
# bash implementation fails due to lack of big integer suuport
#
# external tools used: bc

pow2() { echo "2 ^ ($1 - 1)" | bc; }

shopt -s extglob

case $1 in
    ?(-)+([0-9]) )
        if (($1 <= 0 || $1 > 64)); then 
            echo "Error: invalid input" >&2
            exit 1
        fi
        pow2 $1
        ;;
    total)
        # this implementation, while nice and DRY,
        # is calling bc 65 times:
        #for i in {1..64}; do
        #    pow2 $i
        #done | paste -sd + | bc

        expression="0"
        for i in {1..64}; do
            expression+=" + (2^($i-1))"
        done
        echo "$expression" | bc

        ;;
    *)  echo "Error: invalid input" >&2
        exit 1
        ;;
esac
