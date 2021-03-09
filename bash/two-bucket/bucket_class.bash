#!/usr/bin/env bash

# "Object Oriented" bash programming.
# Inspired by https://unix.stackexchange.com/a/288247/4667
#
# We create a function that represents the class, and
# use a case statement to handle the methods.
#
# Each instance is really just a copy of the function, renamed
# with the instance name. This is, kind of, prototype-based
# OO a la javascript.
#
# Instance variables must be set and retrieved with the `set`
# and `get` methods.

if ! type -t checkBashVersion > /dev/null; then
    # shellcheck source=/dev/null
    source "$(dirname "${BASH_SOURCE[0]}")"/utils.bash
fi
checkBashVersion 4.3 "namerefs"

if ! type -t math::min > /dev/null; then
    # shellcheck source=/dev/null
    source "$(dirname "${BASH_SOURCE[0]}")"/utils_math.bash
fi

# see http://stackoverflow.com/questions/1203583/how-do-i-rename-a-bash-function
alias_function() {
    eval "${1}() $(declare -f "$2" | sed 1d)"
}

Bucket() {
    local class=Bucket
    local instance=${FUNCNAME[0]}
    local instanceData="__${class}_${instance}__"
    local subcommand=$1
    shift

    case $subcommand in
        new)
            instance=$1
            instanceData="__${class}_${instance}__"
            shift
            alias_function "$instance" "$class"

            declare -gA "$instanceData"
            local -n data="$instanceData"

            # "instance variables" -- default values
            data["name"]="aBucket"
            data["size"]=0
            data["amount"]=0

            # user-provided values
            while (($# > 0)); do
                # shellcheck disable=SC2034
                data["$1"]=$2
                shift 2
            done
            ;;
        set)
            local -n _data_set=$instanceData
            # shellcheck disable=SC2034
            _data_set[$1]=$2
            ;;
        get)
            local -n _data_get=$instanceData
            printf '%s' "${_data_get[$1]}"
            ;;
        isFull)
            (( $($instance get amount) == $($instance get size) ))
            ;;
        isEmpty)
            (( $($instance get amount) == 0 ))
            ;;
        freeSpace)
            echo $(( $($instance get size ) - $($instance get amount) ))
            ;;
        fill)
            $instance set amount "$($instance get size)"
            ;;
        empty)
            $instance set amount 0
            ;;
        pourInto)
            local other=$1   # TODO verify $other isa Bucket
            local amount
            amount=$(math::min "$($instance get amount)" "$($other freeSpace)")
            $instance remove "$amount"
            $other add "$amount"
            ;;
        add)
            local amount
            amount=$($instance get amount)
            $instance set amount $((amount + $1))
            ;;
        remove)
            $instance add $((-1 * $1))
            ;;
        toString)
            declare -p "$instanceData"
            ;;
        *)
            die "unknown $class instance method: $subcommand"
            ;;
    esac
}
