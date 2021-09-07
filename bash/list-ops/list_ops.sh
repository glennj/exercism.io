#!/usr/bin/env bash

if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then
    echo "This library of functions should be sourced into another script" >&2
    exit 4
fi
bash_version=$((10 * BASH_VERSINFO[0] + BASH_VERSINFO[1]))
if ((bash_version < 43)); then
    echo "This library requires at least bash version 4.3" >&2
    return 4
fi


# Append some elements to the given list.
list::append () {
    local -n __append_lst=$1
    __append_lst+=("${@:2}")
}


# Return only the list elements that pass the given function.
list::filter () {
    local func=$1
    local -n __filter_lst=$2 __filter_res=$3
    local elem
    for elem in "${__filter_lst[@]}"; do
        if "$func" "$elem" &> /dev/null; then
            __filter_res+=("$elem")
        fi
    done
}

# Transform the list elements, using the given function,
# into a new list.
list::map () {
    local func=$1
    local -n __map_lst=$2 __map_res=$3
    local elem
    for elem in "${__map_lst[@]}"; do
        __map_res+=("$("$func" "$elem")")
    done
}

# Left-fold the list using the function and the initial value.
list::foldl () {
    local func=$1 accum=$2
    local -n __foldl_lst=$3
    local elem
    for elem in "${__foldl_lst[@]}"; do
        accum=$("$func" "$accum" "$elem")
    done
    echo "$accum"
}

# Right-fold the list using the function and the initial value.
list::foldr () {
    local func=$1 accum=$2
    local -n __foldr_lst=$3
    local i
    for ((i = ${#__foldr_lst[@]} - 1; i >= 0; i--)); do
        accum=$("$func" "${__foldr_lst[i]}" "$accum")
    done
    echo "$accum"
}

# Return the list reversed
list::reverse () {
    local -n __rev_lst=$1 __rev_res=$2
    local i
    for ((i = ${#__rev_lst[@]} - 1; i >= 0; i--)); do
        __rev_res+=("${__rev_lst[i]}")
    done
}
