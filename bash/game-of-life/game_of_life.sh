#!/usr/bin/env bash

# declare -ir DEBUG=3

rows=( "$@" )
height=${#rows[@]}
width=${#rows[0]}
# declare -p rows height width >&${DEBUG}

declare -A cells neighbours

for (( i = 0; i < height; i++ )); do
    row=${rows[i]}
    for (( j = 0; j < width; j++ )); do
        cells["$i $j"]=${row:j:1}

        if (( ${row:j:1} == 1 )); then
            for (( di = -1; di <= 1; di++ )); do
                (( ii = i + di ))
                (( 0 <= ii && ii < height )) || continue
                for (( dj = -1; dj <= 1; dj++ )); do
                    (( di == 0 && dj == 0 )) && continue
                    (( jj = j + dj ))
                    (( 0 <= jj && jj < width )) || continue
                    
                    # echo "$i,$j increments $ii,$jj" >&${DEBUG}
                    (( neighbours["$ii $jj"] += 1 ))
                done
            done
        fi
    done
done

# declare -p cells >&${DEBUG}
# declare -p neighbours >&${DEBUG}

for (( i = 0; i < height; i++ )); do
    for (( j = 0; j < width; j++ )); do
        case "${neighbours["$i $j"]}" in
            3) cells["$i $j"]=1 ;;
            2) : ;;
            *) cells["$i $j"]=0 ;;
        esac
        printf '%s' "${cells["$i $j"]}"
    done
    printf '\n'
done
