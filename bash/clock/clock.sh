#!/usr/bin/env bash

source ../lib/utils.bash
source ../lib/utils_string.bash

main() {
    (( $# == 2 || $# == 4 )) || die "invalid arguments"

    str::isInt "$1" || die "non-numeric argument"
    str::isInt "$2" || die "non-numeric argument"

    local -i h=$1 m=$2

    # ensure the hours and minutes are positive
    while ((m < 0)); do ((m += 60, h -= 1)); done
    while ((h < 0)); do ((h += 24)); done

    local -i minutes=$(( 60 * h + m ))

    case $3 in
        [+-]) 
            str::isInt "$4" || die "non-numeric argument"
            minutes=$(( minutes $3 $4 ))
            ;;
        "") : ;;
        *)  die "invalid arguments" ;;
    esac

    # no DST here, all days have 24*60 minutes
    while ((minutes < 0)); do ((minutes += (24*60))); done

    printf "%02d:%02d\n" $(( (minutes / 60) % 24 )) $((minutes % 60))
}

main "$@"
