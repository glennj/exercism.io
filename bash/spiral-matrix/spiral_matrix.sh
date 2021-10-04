#!/usr/bin/env bash

source ./utils.bash
checkBashVersion 4.3 namerefs

# If we were using a 2-D array with, say, n=3
#
# 0 1 2       1 2 3
# 3 4 5   =>  8 9 4
# 6 7 8       7 6 5
#
# Flattening that into a 1-D array:
#
# idx: 0 1 2 3 4 5 6 7 8
# val: 1 2 3 8 9 4 7 6 5

# With the idx initialized to -1, for a length of 3, we
# increment idx by 1; then for a length of 2, we increment
# by 3; then for a length of 2 we increment by -1; then
# for a length of 1 we increment by -3; then for a length of 1
# we increment by 1.
#
# For the length delta, we have a pattern of
# (substract 1, subtract 0) and repeat.
#
# For the index delta, we have a pattern of
# (add 1, add N, subtract 1, subtract N) and repeat.

populate() {
    local -n ary=$1
    local -i n=$2

    local -i _n=$n idx=-1 i=1
    local -i len_delta=1
    local -i idx_delta=1

    while ((i <= n * n)); do
        for ((j = 1; j <= _n; j++)); do
            ((idx += idx_delta))
            # shellcheck disable=SC2034
            ary[idx]=$i
            ((i++))
        done
        ((_n -= len_delta))

        len_delta=$(next_len_delta $len_delta)
        idx_delta=$(next_idx_delta $idx_delta)
    done
}

next_len_delta() { echo $((!$1)); }

next_idx_delta() {
    case $1 in
           1)  printf "%s"  "$n" ;;
          -1)  printf "%s" "-$n" ;;
         "$n") printf "%s"   -1 ;;
        "-$n") printf "%s"    1 ;;
    esac
}

main() {
    if [[ $# -ne 1 ]]; then
        echo "usage: $0 n" >&2
        exit 1
    fi

    # initialize
    local -i n=$(($1))
    local -a spiral

    populate spiral $n

    # output
    for ((i = 0; i < n * n; i += n)); do
        echo "${spiral[*]:i:n}"
    done
}

main "$@"
