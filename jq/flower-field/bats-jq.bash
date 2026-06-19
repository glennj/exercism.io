#!/usr/bin/env bash
#
# `bats-core` will consume both stdout and stderr for the `run` command's output.
# However `jq` prints its DEBUG output on stderr.
#
# Lines starting with `["DEBUG:",` will be prefixed with a hash and printed on file descriptor 3.
# Other lines on stderr will remain on stderr for bats to consume.
#
# See `bats-core` docs:
# - "Printing to the terminal", https://bats-core.readthedocs.io/en/stable/writing-tests.html#printing-to-the-terminal
# - "File descriptor 3", https://bats-core.readthedocs.io/en/stable/writing-tests.html#file-descriptor-3-read-this-if-bats-hangs

jq() {
    local output stderr rc line
    stderr=$(mktemp)
    output=$(command jq "$@" 2> "$stderr")
    rc=$?
    while IFS= read -r line || [[ -n $line ]]; do
        if [[ $line == '["DEBUG:",'* ]]; then
            echo "# $line" >&3
        else
            echo "$line" >&2
        fi
    done < "$stderr"
    rm -f "$stderr"
    echo "$output"
    return "$rc"
}

#############################################################
# These are extra assert functions for use in tests.

# Assert two JSON objects are equal.
#   https://jqlang.org/manual/v1.7/#==-!=
#
#   assert_objects_equal '{"a": 1, "b": 2}' '{"b":2,"a":1}' 
#   # => true
#
assert_objects_equal() {
    local result=$(
        jq -n --argjson actual "$1" \
              --argjson expected "$2" \
            '$actual == $expected'
    )
    [[ $result == "true" ]]
}

# Assert 2 floating-point values are "close enough".
#
#   # are they the same to 2 decimal places?
#   assert_float 1.993 1.995        # => true
#
#   # are they the same to 3 decimal places?
#   assert_float -d 3 1.993 1.995   # => false
#
assert_float() {
    local OPTIND OPTARG
    local decimals=2 actual expected
    while getopts :d: opt; do
        case $opt in
            d) decimals=$OPTARG ;;
            *) return 2 ;;
        esac
    done
    shift $((OPTIND - 1))
    # bash can't do floating point: use awk
    read -r actual expected < <(
        awk -v d="$decimals" -v a="$1" -v e="$2" '
            BEGIN {
                m = 10 ^ d
                print int(a * m)/m, int(e * m)/m
            }
        '
    )
    # now call a bats-assert command to get the desired output
    assert_equal "$actual" "$expected"
}

# Assert that an object's value of a given key is the expected value.
# This uses the bats `output` variable.
#
#   run jq -f ...
#   assert_key_value 'the_key' 'the_expected_value'
#
assert_key_value() {
    local key=$1 expected=$2 actual
    actual=$(jq -rc --arg key "$key" '.[$key]' <<< "$output")
    assert_equal "$actual" "$expected"
}
