#!/bin/bash 

# I know what can be unquoted here:
# shellcheck disable=SC2206
# shellcheck disable=SC2086

declare -A alphaIdx
declare -a alphabet
declare -i m=0
for letter in {a..z}; do 
    alphaIdx[$letter]=$((m++))
    alphabet+=("$letter")
done

declare -i a b mmi

main() {
    (( $# == 4 )) || die "usage: $0 <encode|decode> a b string"
    case "$1" in 
        encode|decode) 
            a=$2
            b=$3
            coprime $a $m || die "a and m must be coprime."
            mmi=$(mmi $a $m) || exit 1
            x_code "$1" "$( echo "$4" | lower_case | alnum_only )"
            ;;
        *) die "Error: unknown subcommand '$1'" ;;
    esac
}

die() { echo "$*" >&2; exit 1; }

x_code() {
    local text=$2 encoded="" char filter func
    case $1 in
        encode) func=E; filter=groups ;;
        decode) func=D; filter=identity ;;
    esac
    local -i i n
    for ((i=0; i<${#text}; i++)); do
        char=${text:i:1}
        if [[ -v alphaIdx[$char] ]]; then
            n=$($func ${alphaIdx[$char]})
            encoded+=${alphabet[n]}
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
    read -ra factors < <(prime_factors $2)
    for f in "${factors[@]}"; do
        (( $1 % f == 0 )) && return 1
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

# These are nice one line pipeline functions that I would
# use in a production script. Let's try them with plain bash.

# external tools used: tr, sed, factor, cat, cut
## identity()   { cat; }
## lower_case() { tr '[:upper:]' '[:lower:]'; }
## alnum_only() { tr -dc "[:alnum:]"; }
## groups()     { sed -Ee "s/.{${1:-5}}/& /g" -e 's/ $//'; }
## prime_factors() { factor "$1" | cut -d ' ' -f 2-; }

identity() {
    while IFS= read -r; do
        echo "$REPLY"
    done
}

lower_case() {
    while IFS= read -r; do
        echo "${REPLY,,}"
    done
}

alnum_only() {
    while IFS= read -r; do
        echo "${REPLY//[^[:alnum:]]/}"
    done
}

groups() {
    local -i size=${1:-5} i
    local -a groups
    while IFS= read -r; do
        groups=()
        for (( i=0; i < ${#REPLY}; i += size )); do
            groups+=( "${REPLY:i:size}" )
        done
        echo "${groups[*]}"
    done
}

prime_factors() {
    local -i n=$1 p=2
    # shortcut
    if (( n == 26 )); then
        echo "2 13"
        return
    fi

    local -a factors=()
    while (( p * p <= n )); do
        if (( n % p == 0 )); then
            factors+=( $p )
            (( n /= p ))
        else
            (( p += 1 ))
        fi
    done
    (( n > 1 )) && factors+=( $n )
    echo "${factors[*]}"
}


main "$@"
