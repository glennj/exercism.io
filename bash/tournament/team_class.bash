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

# external tools used: sed

# shellcheck disable=SC2154,SC2178

if ! type -t checkBashVersion > /dev/null; then
    # shellcheck source=/dev/null
    source "$(dirname "${BASH_SOURCE[0]}")"/utils.bash
fi
checkBashVersion 4.3 "namerefs"

# see http://stackoverflow.com/questions/1203583/how-do-i-rename-a-bash-function
alias_function() {
    eval "${1}() $(declare -f "$2" | sed 1d)"
}

############################################################
Team() {
    local class=Team
    local instance=${FUNCNAME[0]}
    local instanceData="__${class}_${instance}__"
    local subcommand=$1
    shift

    case $subcommand in
        new)
            instance=$1
            if [[ ! $1 =~ ^[[:alnum:]_][[:graph:]]* ]]; then
                printf 'invalid instance name: %q\n' "$1" >&2
                return 1
            fi
            instanceData="__${class}_${instance}__"
            shift
            alias_function "$instance" "$class"

            declare -gA "$instanceData"
            local -n data="$instanceData"

            # "instance variables"
            data["name"]=$1
            data["wins"]=0
            data["losses"]=0
            data["draws"]=0
            data["matches_played"]=0
            data["points"]=0
            ;;

        id)
            local name=$1
            echo "${name//[^[:word:]]/_}"
            ;;

        destroy)
            unset "$instanceData"
            unset -f "$instance"
            ;;

        get)
            local -n data=$instanceData
            echo "${data[$1]}"
            ;;

        win)
            local -n data=$instanceData
            ((data[matches_played] += 1))
            ((data[wins] += 1))
            ((data[points] += 3))
            ;;

        lose)
            local -n data=$instanceData
            ((data[matches_played] += 1))
            ((data[losses] += 1))
            ;;

        draw)
            local -n data=$instanceData
            ((data[matches_played] += 1))
            ((data[draws] += 1))
            ((data[points] += 1))
            ;;

        *)
            die "unknown $class instance method: $subcommand"
            ;;
    esac
}
