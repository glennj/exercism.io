#!/bin/bash

if (( $# != 1 )); then
    echo "usage: $(basename "$0") time-spec" >&2
    exit 1
fi

if ! epoch=$(date --utc --date="$1" "+%s"); then
    echo "error: invalid time-spec '$1'" >&2
    exit 1
fi

# not happy about having to pipe this through sed.
date --utc --date="@$(( epoch + 1000000000 ))" | sed 's/  / /g'
