#!/usr/bin/env bash

# A library of useful bash functions
# Works with bash version 3.2+

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
# v4.2 - some extended globbing fixes
# v4.3 - `-v` operator for `[[...]]`
#      - bug fixes for associative array indices
#      - namerefs
#
checkBashVersion() {
    local -i major minor
    IFS=. read -r major minor <<< "$1"
    local feature=${2:+" for $2"}

    if  ((BASH_VERSINFO[0] < major)) ||
        ((BASH_VERSINFO[0] == major && BASH_VERSINFO[1] < minor))
    then
        die -s 254 "Bash version $1 is required${feature}: this is $BASH_VERSION"
    fi
}

# Return the major.minor Bash version as an integer:
# 10 * major + minor
# This will help with comparing versions using integer comparison
# (as opposed to float comp or lexical comp)
#
bashversion() {
    echo $((10 * BASH_VERSINFO[0] + BASH_VERSINFO[1] ))
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

# Evaluate an arithmetic expression or a command.
# On failure, error and exit.
# - arithmetic failure: expression results in value zero.
# - command failure: non-zero exit status.
# e.g.
#   assert "$# > 0" "Provide at least one argument"
#   assert -C grep -q pattern file "pattern not found in file"
#
# Be careful with the arithmetic expressions:
#   var='-1'
#   # ......▽
#   assert 'var > 0'  'err' # properly dies
#   assert "$var > 0" 'err' # shows usage
#   # ......△
# This is because getopts sees "-1 > 0" as an option,
# the *) case ignores it but getopts increments OPTIND.
# Then $1 gets shifted off and the (($# < 2)) test fails.
#
assert() {
    local mode=A
    local OPTIND OPTARG opt
    while getopts :hC opt; do
        case $opt in
            h)  assert::usage
                return
                ;;
            C)  mode=C ;;
            *)  : ;;
        esac
    done
    shift $((OPTIND - 1))
    if (($# < 2)); then
        assert::usage
        return 1
    fi

    local msg="${*: -1:1}"
    local args=("${@:1:$# - 1}")
    case $mode in
        A)  # evaluate as arithmetic
            (("${args[@]}")) || die "$msg"
            ;;
        C)  # evaluate as a command
            "${args[@]}" || die "$msg"
            ;;
    esac
}

assert::usage() {
    # caution: literal tabs below
    cat >&2 <<- END_USAGE
		Usage: ${FUNCNAME[1]} [-h] [-C] arg [...] "message"
		
		Without -C, 'arg ...' is an arithmetic expression to evaluate.
		With    -C, 'arg ...' is a command to execute.
	END_USAGE
}

# The opposite of `assert`: fail if the expression is _true_
refute() {
    local msg=${*: -1:1}
    # Need to `assert` in a subshell to absorb an `exit`.
    # Use a command substitution to capture any output.
    # Die if assert returns successfully:
    if _=$(assert "$@" 2>&1); then
        die "$msg"
    fi
}

# Do something with a shell setting _temporarily_ set
#
# Example: trim leading zeros
#     n="00123"
#     with_shopt extglob 'echo "${n##+(0)}"'
#
# Beware: `eval` resides within.
#
with_shopt() {
    local setting=$1 script=$2
    local current_setting
    read -ra current_setting < <(shopt -p "$setting")
    shopt -s "$setting"

    eval "$script"
    local rc=$?

    "${current_setting[@]}"

    return "$rc"
}

# Return a "boolean string" based on the _last_ exit status.
#
# Example:
#   ((some_expression == some_value))
#   true_or_false
#
true_or_false() {
    # shellcheck disable=SC2181
    (($? == 0)) && echo true || echo false
}
