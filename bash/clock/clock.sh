#!/usr/bin/env bash

source ./utils.bash
source ./utils_string.bash
source ./utils_math.bash

hm_to_m() {
    local mins=$((60 * $1 + $2))
    # no DST here, all days have 24*60 minutes
    math::floorMod $mins 1440
}

main() {
    local minutes other

    assert "$# >= 2" "invalid arguments"
    assert -C str::isInt "$1" "non-numeric argument"
    assert -C str::isInt "$2" "non-numeric argument"

    minutes=$(hm_to_m "$1" "$2")

    case $3 in
        =)
            assert "$# == 5" "invalid arguments"
            assert -C str::isInt "$4" "non-numeric argument"
            assert -C str::isInt "$5" "non-numeric argument"

            other=$(hm_to_m "$4" "$5")
            ((minutes == other)) && echo true || echo false
            return
            ;;
        [+-])
            assert "$# == 4" "invalid arguments"
            assert -C str::isInt "$4" "non-numeric argument"

            # shfmt and shellcheck both have problems with
            # dynamically generated expressions like this:
            # shellcheck disable=SC1105,SC2086,SC1102
            minutes=$(math::floorMod "$((minutes $3 $4))" 1440)
            ;;
        "")
            assert "$# == 2" "invalid arguments"
            ;;
        *)  die "invalid arguments" ;;
    esac

    printf "%02d:%02d\n" $((minutes / 60 % 24)) $((minutes % 60))
}

main "$@"
