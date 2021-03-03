#!/usr/bin/env bash

# a library of useful bash functions
# works with bash version 3.2+

#############################################################
# Script-level functions

# Check the bash version.
# Scripts that rely on bash features introduced in a
# particular version should validate they're running with the
# necessary version.
#
# parameters:
# - version number in <major>.<minor> form
# - name of feature requiring that version
#
# e.g. checkBashVersion 4.3 namerefs
#
# the introduction of some intesting bash features:
# v4.0 - case conversion parameter expansion and `declare -u/-l`
#      - associative arrays
#      - a bug fix for array concatenation
#      - `mapfile`
# v4.1 - `printf -v arrray[index]`
# v4.3 - `-v` operator for `[[...]]`
#      - bug fixes for associative array indices
#      - namerefs
#
checkBashVersion() {
    local -i major minor
    IFS=. read -r major minor <<<"$1"
    local feature=${2:+" for $2"}

    if  (( BASH_VERSINFO[0] < major )) ||
        (( BASH_VERSINFO[0] == major && BASH_VERSINFO[1] < minor ))
    then
        die -s 254 "Bash version $1 is required${feature}: this is $BASH_VERSION"
    fi
}


# Evaluate an arithmetic expression.
# If it results in 0 (or false), error and exit.
# e.g.
#   assert "$# > 0" "Provide at least one argument"
#
assert() {
    (( $1 )) || die "$2"
}


# Emit an error message and exit
# e.g.:  [[ $x == $y ]] || die "Incorrect value"
# 
# Provide an exit status with the -s option (default: 1)
#
die() { 
    local status=1
    local OPTIND OPTARG
    while getopts s: opt; do
        [[ $opt == "s" ]] && status=$OPTARG
    done
    echo "${*:OPTIND}" >&2
    exit "$status"
}
