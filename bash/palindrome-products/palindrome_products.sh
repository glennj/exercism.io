#!/usr/bin/env bash

source ./utils.bash
source ./utils_string.bash

# algorithm lifted from
# https://exercism.io/tracks/python/exercises/palindrome-products/solutions/d68cab86cad94d4d821f26da44bb0722

factors() {
    local -i n=$1 min=$2 max=$3 step=$4 i j start
    local factors="" cond
    case $step in
        1) start=min; cond="<=" ;;
        *) start=max; cond=">=" ;;
    esac
    # the dynamic expression gives shellcheck and shfmt trouble.
    for ((i = start; i * i $cond n; i += step)); do
        j=$((n / i))
        if ((i * j == n && (min <= j && j <= max))); then
            case $step in
                1) factors+=" [$i, $j]" ;;
                *) factors+=" [$j, $i]" ;;
            esac
        fi
    done
    echo "$factors"
}

main() {
    local which=$1
    local -i min=$2 max=$3

    ((min > max)) && die "min must be <= max"

    local -i step
    local -i start end
    case $which in
        smallest)
            step=1
            start=$((min ** 2))
            end=$((max ** 2))
            ;;
        largest)
            step=-1
            start=$((max ** 2))
            end=$((min ** 2))
            ;;
        *) die "first arg should be 'smallest' or 'largest'" ;;
    esac

    local -i i=$start
    local factors
    while ((step > 0 && i <= end)) || ((step < 0 && i >= end)); do
        if str::isPalindrome "$i"; then
            # shellcheck disable=SC2086
            factors=$(factors $i $min $max $step)
            if [[ -n $factors ]]; then
                printf "%d:%s\n" $i "$factors"
                break
            fi
        fi
        ((i += step))
    done
}

main "$@"

