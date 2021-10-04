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
Tournament() {
    local class=Tournament
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
            data["teams"]=""
            data["fmt"]="%-30s | %2s | %2s | %2s | %2s | %2s\n"

            # validate
            # * [[ -t 0 ]] means "is file descriptor 0 (aka stdin)
            #   attached to a terminal"
            # * if this is true, then there is no input redirection
            #   and we expect to be given a results file
            if [[ -t 0 ]]; then
                if [[ -z $1 ]]; then
                    die "no results file provided"
                elif [[ ! -f $1 ]] || [[ ! -r $1 ]]; then
                    die "file '$1' does not exist or is unreadable"
                fi
                # we have a file to read
                # attach it to stdin
                exec 0<"$1"
            fi
            ;;

        destroy)
            unset "$instanceData"
            unset -f "$instance"
            ;;

        get)
            local -n data=$instanceData
            echo "${data[$1]}"
            ;;

        process)
            # we're reading from stdin
            while IFS=';' read -r home away result; do
                [[ $home && $away && $result ]] || continue

                # since objects are functions, we need a valid
                # function name. Get the class to provide one.
                home_id=$(Team id "$home")
                $instance register "$home_id" "$home"

                away_id=$(Team id "$away")
                $instance register "$away_id" "$away"

                case $result in
                    win)  "$home_id"  win; "$away_id" lose ;;
                    loss) "$home_id" lose; "$away_id"  win ;;
                    draw) "$home_id" draw; "$away_id" draw ;;
                esac
            done
            ;;

        register)
            local -n data=$instanceData
            local id=$1 name=$2
            if [[ $(type -t "$id") != "function" ]]; then
                Team new "$id" "$name" 
                data[teams]+="${data[teams]:+$'\034'}${id}"
            fi
            ;;

        results)
            local -n data=$instanceData
            local -a teams
            IFS=$'\034' read -ra teams <<<"${data[teams]}"

            printf "${data[fmt]}" Team MP W D L P
            for team_id in "${teams[@]}"; do
                printf "${data[fmt]}" \
                    "$("$team_id" get name)" \
                    $("$team_id" get matches_played) \
                    $("$team_id" get wins) \
                    $("$team_id" get draws) \
                    $("$team_id" get losses) \
                    $("$team_id" get points)
            done | sort -t "|" -k6,6nr -k1,1
            ;;
        *)
            die "unknown $class instance method: $subcommand"
            ;;
    esac
}
