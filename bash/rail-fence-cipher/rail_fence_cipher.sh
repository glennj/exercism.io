#!/usr/bin/env bash

source ./utils.bash
source ./utils_string.bash
checkBashVersion 4.3 namerefs

# the quick brown fox jumps over the lazy dog
# 5 rails
#
# t       k       f       s       h       d    : tkfshd
#  h     c _     _ o     p _     t e     _ o   : hc__co_te_o
#   e   i   b   n   x   m   o   _   _   y   g  : eibnxmo__yg
#    _ u     r w     _ u     v r     l z       : _urw_uvrlz
#     q       o       j       e       a        : qojea
#
#
# the quick brown fox jumps over the lazy dog
# tkfshdhc  co te oeibnxmo  yg urw uvrlzqojea

__code() {
    local type=$1
    local -i n=$2
    local src=$3 len=${#3}
    local -i idx i j k=0

    # length of a "down & up cycle"
    local -i cycle=$((2 * (n - 1)))

    # a string spaces of the appropriate length
    local dest
    printf -v dest "%*s" "$len" ""

    for ((i = 0; i < n; i++)); do
        for ((j = 0; j <= len / n; j++)); do
            idx=$((i + j * cycle))
            ((idx >= len)) && break
            case $type in
                E) str::putAt $((k++)) "${src:idx:1}" dest ;;
                D) str::putAt $idx     "${src:k++:1}" dest ;;
            esac

            # "middle" rails consume 2 chars per cycle
            if ((0 < i && i < n - 1)); then
                idx=$(((cycle - i) + j * cycle))
                ((idx >= len)) && break
                case $type in
                    E) str::putAt $((k++)) "${src:idx:1}" dest ;;
                    D) str::putAt $idx     "${src:k++:1}" dest ;;
                esac
            fi
        done
    done

    echo "$dest"
}

main() {
    local OPTIND OPTARG
    local type
    local -i rails

    while getopts :e:d: opt; do
        case $opt in
            e) type=E; rails=$OPTARG;;
            d) type=D; rails=$OPTARG;;
            :) die "missing argument for -$OPTARG";;
            ?) die "unknown option -$OPTARG";;
        esac
    done
    shift $((OPTIND - 1))

    assert "${#type}" "specify -e or -d"
    assert "rails > 0" "must have positive number of rails"

    __code "$type" "$rails" "$1"
}

main "$@"
