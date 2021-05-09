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

# external tools used: mktemp, awk

if ! type -t checkBashVersion > /dev/null; then
    # shellcheck source=/dev/null
    source "$(dirname "${BASH_SOURCE[0]}")"/utils.bash
fi
checkBashVersion 4.3 "namerefs"

if ! type -t math::gcd > /dev/null; then
    # shellcheck source=/dev/null
    source "$(dirname "${BASH_SOURCE[0]}")"/utils_math.bash
fi

# This global variable holds the instance name of the last
# object created
declare RATIONAL_NUMBER_INSTANCE

# see http://stackoverflow.com/questions/1203583/how-do-i-rename-a-bash-function
alias_function() {
    eval "${1}() $(declare -f "$2" | sed 1d)"
}

# TODO make it not rely on mktemp
randName() {
    local f
    f=$(mktemp)
    rm "$f"
    f="${f##*/}"
    echo "${f//[^[:alnum:]_]/_}"
}

############################################################
RationalNumber() {
    local class=RationalNumber
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
            RATIONAL_NUMBER_INSTANCE=$instance
            instanceData="__${class}_${instance}__"

            shift
            alias_function "$instance" "$class"

            declare -gA "$instanceData"
            local -n data="$instanceData"

            # "instance variables" -- default values
            data["numerator"]=0
            data["denominator"]=1

            # user-provided values
            if (($# == 1)) && [[ $1 =~ ([+-]?[0-9]+)/([+-]?[0-9]+) ]]; then
                data["numerator"]=$((BASH_REMATCH[1]))
                data["denominator"]=$((BASH_REMATCH[2]))
            elif (($# == 2)); then
                data["numerator"]=$(($1))
                # shellcheck disable=SC2034  # why just here?
                data["denominator"]=$(($2))
            fi
            "$instance" reduce
            ;;

        create)
            $class new "$(randName)" "$@"
            ;;

        newOrCreate)
            # Depending on the presence of the `-v name`
            # option, instantiate a new named instance, or
            # create an unnamed one.
            local instanceName opt OPTIND OPTARG
            while getopts :v: opt; do
                [[ $opt == "v" ]] && instanceName=$OPTARG
            done
            shift $((OPTIND - 1))
            if [[ -n $instanceName ]]; then
                $class new "$instanceName" "$@"
            else
                $class create "$@"
            fi
            ;;

        destroy)
            unset "$instanceData"
            unset -f "$instance"
            ;;

        reduce)
            local -n _data_reduce=$instanceData
            local n="${_data_reduce[numerator]}"
            local d="${_data_reduce[denominator]}"
            local -i gcd
            gcd=$(math::gcd "$n" "$d")
            ((n /= gcd))
            ((d /= gcd))
            if ((d < 0)); then
                ((n *= -1))
                ((d *= -1))
            fi
            _data_reduce[numerator]=$n
            _data_reduce[denominator]=$d
            ;;

        get)
            local -n _data_get=$instanceData
            echo "${_data_get[$1]}"
            ;;

        # convenience methods
        numerator)   "$instance" get numerator ;;
        denominator) "$instance" get denominator ;;

        toString)
            printf "%d/%d\n" "$("$instance" numerator)" "$("$instance" denominator)"
            ;;

        [-+*/])
            local other=$1   # TODO verify $other isa RationalNumber
            shift
            local n1 d1 n2 d2 n3 d3
            n1=$("$instance" numerator)
            d1=$("$instance" denominator)
            n2=$("$other" numerator)
            d2=$("$other" denominator)

            case $subcommand in
                +)  n3=$((n1 * d2 + n2 * d1))
                    d3=$((d1 * d2))
                    ;;
                -)  n3=$((n1 * d2 - n2 * d1))
                    d3=$((d1 * d2))
                    ;;
              '*')  n3=$((n1 * n2))
                    d3=$((d1 * d2))
                    ;;
                /)  n3=$((n1 * d2))
                    d3=$((d1 * n2))
                    ;;
            esac

            $class newOrCreate "$@" -- $n3 $d3
            ;;

        abs)
            local new=(
                "$(math::abs "$("$instance" numerator)")"
                "$(math::abs "$("$instance" denominator)")"
            )
            $class newOrCreate "$@" -- "${new[@]}"
            ;;

        pow)
            # this is rational ^ real => 2/3 ^ 3 == 8/27
            local power=$1
            shift
            local n d
            n=$("$instance" numerator)
            d=$("$instance" denominator)
            if ((power >= 0)); then
                n=$((n ** power))
                d=$((d ** power))
            else
                n=$((d ** (-1 * power)))
                d=$((n ** (-1 * power)))
            fi
            $class newOrCreate "$@" -- $n $d
            ;;

        rpow)
            # this is real ^ rational => 3 ^ 1/2 == sqrt(3)
            # bail out to an external tool
            awk -v a="$1" \
                -v b="$("$instance" numerator)" \
                -v c="$("$instance" denominator)" \
                'BEGIN {
                    # want to limit to 6 decimal places
                    ans = sprintf("%.6f", a ** (b / c))

                    # but do not want to print 1.0 as 1.000000
                    sub(/0+$/, "0", ans)
                    print ans
                }'
            ;;

        *)
            die "unknown $class instance method: $subcommand"
            ;;
    esac
}
