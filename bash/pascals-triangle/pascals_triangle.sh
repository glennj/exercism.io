#!/usr/bin/env bash

source ../lib/utils.bash
checkBashVersion 4.0 "associative arrays"

############################################################
# a 2-D array approach. While bash does not have
# multidimensional arrays,  we can fake them using an
# associative array and specially constructed index strings.

using2DArray() {
    local -A cells
    cells[0, 1]=1

    local -i n=$1

    for ((i = 1; i <= n; i++)); do
        # print the leading spaces for this row
        printf "%*s" $((n - i)) ""

        row=()
        for ((j = 1; j <= i; j++)); do
            printf -v a "%d, %d" $((i - 1)) $((j - 1))
            printf -v b "%d, %d" $((i - 1)) $((j))

            cells[$i, $j]=$((${cells[$a]:-0} + ${cells[$b]:-0}))

            row+=("${cells[$i, $j]}")
        done
        echo "${row[*]}"
    done
}

############################################################
# A mathematical approach: each cell can be calculated
# independently: row n column k value is the binomial
# coefficient C(n,k) = n! / (k! * (n-k)!)
#
# This approach limits us to about 21 rows maximum, after
# that we get integer overflows calculating the factorial.
# This will also be much slower than the 2-D array approach.
#
# In order to effectively cache the factorials (which are
# expensive to calculcate repeatedly), we must avoid invoking
# the `binomialCoefficient` and `factorial` functions with
# command substitutions. Command substitution syntax adds
# a level of subshell, and variables set in subshells do not
# persist into the current shell, making caching impossible.

usingFactorial() {
    # Note, local variables are visible here plus in
    # functions called from here
    local -A cacheBC
    local -a cacheFact=([0]=1 [1]=1)
    local -i maxCachedFact=1

    local -i n=$1 i
    for ((i = 1; i <= n; i++)); do
        # print the leading spaces for this row
        printf "%*s" $((n - i)) ""
        row "$i"
    done
}

row() {
    local -i n=$1 k _n _k
    local -a row=()
    for ((k = 1; k <= n; k++)); do
        _n=$((n - 1))
        _k=$((k - 1))
        binomialCoefficient "$_n" "$_k"
        row+=("${cacheBC[$_n, $_k]}")
    done
    echo "${row[*]}"
}

# does not return a value, merely populates the cache
binomialCoefficient() {
    local -i n=$1 k=$2
    if ((k == 0 || n - k == 0)); then
        cacheBC[$n, $k]=1
    elif ((k == 1 || n - k == 1)); then
        cacheBC[$n, $k]=$n
    else
        # since n >= k, calling `factorial $n` will
        # populate the cacheFact for `k` and `n-k`
        factorial "$n"

        cacheBC[$n, $k]=$(( \
            cacheFact[n] / (cacheFact[k] * cacheFact[n - k])))
    fi
}

# does not return a value, merely populates the cache
factorial() {
    local -i n=$1
    if ((n > maxCachedFact)); then
        local -i i f=${cacheFact[maxCachedFact]}
        for ((i = maxCachedFact + 1; i <= n; i++)); do
            ((f *= i))
            cacheFact[i]=$f
        done
        maxCachedFact=$n
    fi
}

############################################################
benchmarking=false
func=usingFactorial

while getopts :Bab opt; do
    case $opt in
        B) benchmarking=true ;;
        a) func=using2DArray ;;
        b) func=usingFactorial ;;
        *) : ;;
    esac
done
shift $((OPTIND - 1))

if ! $benchmarking; then
    "$func" "$@"
else
    # benchmarking with `hyperfine`
    export -f using2DArray usingFactorial
    hyperfine --warmup 10 --min-runs 100 \
        -n 2dArray    'bash pascals_triangle.sh -a 21' \
        -n factorials 'bash pascals_triangle.sh -b 21'
fi
