#!/usr/bin/env bash

source ./utils.bash
source ./utils_string.bash
source ./utils_math.bash

main() {
    assert "$# == 2 || $# == 4" "invalid arguments"
    assert -C str::isInt "$1" "non-numeric argument"
    assert -C str::isInt "$2" "non-numeric argument"

    local -i minutes=$((60 * $1 + $2))

    case $3 in
        [+-])
            assert -C str::isInt "$4" "non-numeric argument"

            # shfmt and shellcheck both have problems with
            # dynamically generated expressions like this:
            # shellcheck disable=SC1105,SC2086
            ((minutes ${3}= $4))
            ;;
        "") : ;;
        *)  die "invalid arguments" ;;
    esac

    # no DST here, all days have 24*60 minutes
    minutes=$(math::floorMod $minutes 1440)

    printf "%02d:%02d\n" $((minutes / 60 % 24)) $((minutes % 60))
}

main "$@"
