#!/usr/bin/env bash

source ../lib/utils.bash
source ../lib/utils_math.bash
checkBashVersion 4.0 "associative arrays"

phrase=$1
declare -i rotation=$2

# this is necessary for negative rotation
rotation=$( math::floorMod $rotation 26 )

alphabet=( {a..z} )
rotated=( "${alphabet[@]:rotation}" "${alphabet[@]:0:rotation}" )

## using `tr`:
# from=$(IFS=; echo "${alphabet[*]}${alphabet[*]^^}")
# to=$(IFS=; echo "${rotated[*]}${rotated[*]^^}")
# tr "$from" "$to" <<<"$phrase"

# using plain bash:
declare -A mapping
for (( i=0; i < ${#alphabet[@]}; i++ )); do
    mapping[${alphabet[i]}]=${rotated[i]}       # lower case
    mapping[${alphabet[i]^^}]=${rotated[i]^^}   # upper case
done

result=""
for (( i=0; i < ${#phrase}; i++ )); do
    char=${phrase:i:1}
    if [[ -n ${mapping[$char]} ]]; then
        result+=${mapping[$char]}
    else
        result+=$char
    fi
done

echo "$result"
