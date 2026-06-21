#!/usr/bin/env bash

# day of week numbers, corresponding to `date +%u`
declare -ir MON=1 WED=3 FRI=5 SUN=7
# last month of each quarter
declare -ar Qmonths=( [1]=3 [2]=6 [3]=9 [4]=12 )

die () { echo "$*" >&2; exit 1; }

splitDateTime () {
    # populates the `date` and `time` variables in the caller's scope
    IFS="T" read -r date time <<< "$1"
}

currentYearMonth () {
    # populates the `year` and `month` variables in the caller's scope
    read -r year month <<< "$(date -d "$date" "+%Y %-m")"
}

# ------------------------------------------------
NOW () {
    local date time
    splitDateTime "$1"
    # `date`s parsing is weird. A "+" after the time is considered a timezone offset
    date -d "${date} + 2 hours ${time}" "+%FT%T"
}

# ------------------------------------------------
ASAP () {
    local date time
    splitDateTime "$1"
    if [[ "$time" < "13:00:00" ]]; then
        # 5PM today
        date -d "${date} 17:00:00" "+%FT%T"
    else
        # 1PM tomorrow
        date -d "${date} + 1 day 13:00:00" "+%FT%T"
    fi
}

# ------------------------------------------------
EOW () {
    local date time dayOfWeek dueDay dueTime
    splitDateTime "$1"
    dayOfWeek=$(date -d "$date" '+%u')
    if (( dayOfWeek <= WED )); then
        dueDay=$FRI
        dueTime="17:00:00"
    else
        dueDay=$SUN
        dueTime="20:00:00"
    fi
    date -d "$date + $((dueDay - dayOfWeek)) days $dueTime" "+%FT%T"
}

# ------------------------------------------------
months () {
    local targetMonth date time year month dayOfWeek dueDate
    targetMonth=${1%M}
    splitDateTime "$2"
    currentYearMonth

    (( month < targetMonth )) || (( ++year ))
    dueDate=$(date -d "${year}-${targetMonth}-01" "+%F")
    # Find the first _working_ day of the target month
    while true; do
        dayOfWeek=$(date -d "$dueDate" "+%u")
        (( MON <= dayOfWeek && dayOfWeek <= FRI )) && break
        dueDate=$(date -d "$dueDate + 1 day" "+%F")
    done

    echo "${dueDate}T08:00:00"
}

# ------------------------------------------------
quarter () {
    local quarter date time year month dayOfWeek dueDate
    quarter=${1#Q}
    splitDateTime "$2"
    currentYearMonth

    # last day of the last month in the quarter
    (( month <= Qmonths[quarter] )) || (( ++year ))
    dueDate=$(date -d "${year}-${Qmonths[quarter]}-01 + 1 month - 1 day" "+%F")
    # find the last _working_ day
    while true; do
        dayOfWeek=$(date -d "$dueDate" "+%u")
        (( MON <= dayOfWeek && dayOfWeek <= FRI )) && break
        dueDate=$(date -d "$dueDate - 1 day" "+%F")
    done

    echo "${dueDate}T08:00:00"
}

# ------------------------------------------------
main () {
    case "$1" in
        NOW | ASAP | EOW) "$@" ;;
        *M)               months "$@" ;;
        Q[1234])          quarter "$@" ;;
        *)                die "Invalid subcommand" ;;
    esac
}

main "$@"
