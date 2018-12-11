#!/bin/bash 
# external tools used: tr, sed, factor, cat

main() {
    if (( $# != 4 )); then
        echo "usage: $(basename "$0") <encode|decode> a b string" >&2
        exit 1
    fi
    case "$1" in 
        encode|decode) 
            declare -gi a=$2 b=$3
            if ! coprime $a $m; then
                echo "a and m must be coprime." >&2
                exit 1
            fi
            declare -gi mmi=$(mmi $a $m) || exit 1
            x_code "$1" "$( echo "$4" | lower_case | alnum_only )"
            ;;
        *) echo "Error: unknown subcommand '$1'" >&2; exit 1;;
    esac
}

x_code() {
    local text=$2 encoded="" char filter func
    case $1 in
        encode) func=E; filter=groups ;;
        decode) func=D; filter=cat    ;;
    esac
    local -i i n
    for ((i=0; i<${#text}; i++)); do
        char=${text:i:1}
        if [[ ${a2i[$char]} ]]; then
            n=$($func ${a2i[$char]})
            encoded+=${alphabet:n:1}
        else
            encoded+=$char
        fi
    done
    echo "$encoded" | $filter
}

E() { echo $(( (a * $1 + b) % m )); }
D() { echo $(( mmi * ($1 - b) % m )); }

coprime() { 
    (( $# == 2 )) || return 1
    local factors f
    read -ra factors < <(factor $2)
    for f in "${factors[@]:1}"; do
        (( $1 % $f == 0 )) && return 1
    done
    return 0
}

mmi() { 
    (( $# == 2 )) || return 1
    local -i i
    for ((i=1; i<=$2; i++)); do 
        if (( ($1 * i) % $2 == 1 )); then
            echo $i
            return 0
        fi
    done
    echo "Error: cannot find MMI of $1 mod $2" >&2
    return 1
}

lower_case() { tr '[:upper:]' '[:lower:]'; }
alnum_only() { tr -dc "[:alnum:]"; }
groups()     { sed -Ee "s/.{${1:-5}}/& /g" -e 's/ $//'; }

alphabet=abcdefghijklmnopqrstuvwxyz
m=${#alphabet}
declare -A a2i
for ((i=0; i<m; i++)); do a2i[${alphabet:i:1}]=$i; done

main "$@"
