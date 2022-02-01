#!/usr/bin/env bash

# I can take a relaxed approach to quoting
# shellcheck disable=SC2086

# This solution tries to minimize the number of external
# calls to `date`. There are up to 4:
# - 2 to determine the weekday name of the first of the given month
# - 2 to determine the last date of the given month (only if needed)
#
# All the rest is done with arithmetic.

source ./utils_date.bash
source ./utils_array.bash

declare -A START_DAY=(
    [first]=1       # first falls in the 1..7 of the month
    [second]=8      # second falls in the 8..14 of the month
    [third]=15      # third falls in the 15..21 of the month
    [fourth]=22     # fourth falls in the 22..28 of the month
    [fifth]=29      # fifth falls on or after the 29 of the month
    [teenth]=13     # teenth falls in the 13..19 of the month
    [last]=TBD      # last falls on or after the 22 of the month,
                    # depending on the month
)

declare WEEKDAYS=(Sunday Monday Tuesday Wednesday Thursday Friday Saturday)

# find the last date of the given month
last_date() {
    local year=$1 month=$2
    local date epoch last_date

    # first, get the first of _next_ month
    case $month in
        12) printf -v date '%d-01-01' $((year + 1)) ;;
         *) printf -v date '%d-%d-01' $year $((month + 1)) ;;
    esac
    epoch=$(date::parse -f '%Y-%m-%d' "$date")

    # then subtract one day
    last_date=$(date::format -f '%d' $((epoch - 86400)))

    # and the last *day of the month falls on or after 6 days before
    START_DAY[last]=$((last_date - 6))
}

# find the weekday of the first of the given month
first_weekday() {
    local year=$1 month=$2
    local date epoch
    epoch=$(date::parse -f '%Y-%m-%d' "$year-$month-01")
    date::format -f '%A' $epoch
}

main() {
    local year=$1 month=$2 nth=$3 weekday=$4
    local day offset

    offset=$(array::index WEEKDAYS "$(first_weekday $year $month)")

    case $nth in
        last)
            last_date $year $month
            offset=$(((offset + START_DAY['last'] % 7 - 1) % 7))
            ;;
        teenth)
            offset=$(((offset + START_DAY['teenth'] % 7 - 1) % 7))
            ;;
    esac

    day=${START_DAY[$nth]}

    for ((d = 0; d < 7; d++)); do
        if [[ ${WEEKDAYS[(offset + d) % 7]} == "$weekday" ]]; then
            printf '%d-%02d-%02d\n' $year $month $((day + d))
            break
        fi
    done
}

main "$@"
