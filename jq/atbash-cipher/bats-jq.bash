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
