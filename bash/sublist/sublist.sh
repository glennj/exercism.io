#!/usr/bin/env bash

# print the result, and exit success
result() {
    echo "$*"
    exit 0
}

############################################################
# Solution 1: An approach using string comparison:
# * parse the incoming JSON array string into a string with
#   array elements separated by a character _unlikely_ to
#   appear in the data
# * then do comparisons with string/substring matching.
#
# However, the join character *might* be in the data,
# which could lead to incorrect results.

main__comparing_as_strings() {
    local list1=$( normalize "$1" )
    local list2=$( normalize "$2" )

    [[ $list1 == "$list2" ]] && result equal

    isempty "$list1" && result sublist
    isempty "$list2" && result superlist

    [[ $list1 == *"$list2"* ]] && result superlist
    [[ $list2 == *"$list1"* ]] && result sublist

    result unequal
}

# \034 is the ASCII FS (field separator) character
# It's unlikely to appear in the input text.
FS=$'\034'

normalize() {
    local list=$1
    list=${list//, /$FS}
    list=${list/#[/$FS}
    list=${list/%]/$FS}
    echo "$list"
}

isempty() { [[ $1 == "${FS}${FS}" ]]; }


############################################################
# Solution 2: A list-based approach
# * parse the incoming JSON array strings into bash arrays
# * then use array lengths and iterations over the lists
#   to determine the result

main__comparing_list_elements() {
    local -a list1 list2

    parse_json_array "$1" list1
    parse_json_array "$2" list2

    local -i len1=${#list1[@]}
    local -i len2=${#list2[@]}

    # simple cases
    (( len1 == 0 && len2 == 0 )) && result equal
    (( len1 == 0 )) && result sublist
    (( len2 == 0 )) && result superlist

    if (( len1 < len2 )); then
        contained_in list1 list2 && result sublist   || result unequal

    elif (( len1 > len2 )); then
        contained_in list2 list1 && result superlist || result unequal

    else
        local -i i
        for (( i=0; i<len1; i++)); do
            [[ ${list1[i]} != "${list2[i]}" ]] && result unequal
        done
        result equal
    fi
}

# check if A is contained in B (A is shorter than B)
contained_in() {
    local -n a=$1 b=$2
    local result=$3
    local -i lenA=${#a[@]} lenB=${#b[@]} iA iB
    local found
    iB=0
    while (( iB <= lenB - lenA )); do
        if [[ ${a[0]} == ${b[iB]} ]]; then
            found=true
            for (( iA=1; iA < lenA; iA++)); do
                if [[ ${a[iA]} != ${b[iA+iB]} ]]; then
                    found=false
                    break
                fi
            done
            $found && return 0  # yes, contained in
        fi
        ((iB++))
    done
    return 1    # not contained in
}

parse_json_array() {
    local json=$1
    local -n list=$2

    if type -p jq >/dev/null; then
        parse_json_array_with_jq "$json" list
    else
        parse_json_array_simple "$json" list
    fi
}

# This is a more robust approach, for parsing any
# 1-dimensional JSON array.
parse_json_array_with_jq() {
    set -e
    local json=$1
    local -n __ary=$2
    IFS=$'\t' read -ra __ary < <(jq -r '@tsv' <<<"$json")
    set +e
}

# This is a very simple way to parse the incoming JSON:
# only look for numbers. This is very specific to the
# test data.
parse_json_array_simple() {
    local json=$1
    local -n __ary=$2
    __ary=()
    while [[ $json =~ [^[:digit:]]+([[:digit:]]+) ]]; do
        __ary+=( "${BASH_REMATCH[1]}" )
        json=${json#${BASH_REMATCH[0]}}
    done
}

############################################################

#main__comparing_as_strings "$@"
main__comparing_list_elements "$@"



############################################################
# very rudimentary benchmarking
#
# 1. using string comparison
#
# $ time sh -c 'BATS_RUN_SKIPPED=true bats *_test.sh'
# ...
# 17 tests, 0 failures
#         0.50 real         0.30 user         0.21 sys
#
# 2. using list iteration
#
# $ time sh -c 'BATS_RUN_SKIPPED=true bats *_test.sh'
# ...
# 17 tests, 0 failures
#         1.31 real         0.69 user         0.22 sys

