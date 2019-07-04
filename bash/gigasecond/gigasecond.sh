#!/usr/bin/env bash

if (( $# != 1 )); then
    echo "usage: $(basename "$0") time-spec" >&2
    exit 1
fi

gigasecond=1000000000

if { date --version | grep -q "GNU coreutils"; } 2>/dev/null; then

    if ! epoch=$(date --utc --date="$1" "+%s"); then
        echo "error: invalid time-spec '$1'" >&2
        exit 1
    fi

    input="$1 +$gigasecond second"

    output=$( date --utc --date="$input" )

elif [[ $(what "$(type -P date)") == *"PROGRAM:date"*"PROJECT:shell_cmds"* ]]; then
    # Possibly MacOS date.
    # This BSD-derived date is less flexible about date parsing.

    input=${1%Z}
    format="%Y-%m-%d %H:%M:%S"

    epoch() { 
        date -juf "$format" "$1" "+%s" 2>/dev/null
    }

    if ! e=$(epoch "$input"); then
        # the input date may only be YYYY-mm-dd: add "midnight"
        if ! e=$(epoch "$input 00:00:00"); then
            echo "error: invalid time-spec '$1'" >&2
            exit 1
        fi
    fi

    output=$( date -juf "%s" "$((e + gigasecond))" )

fi

# this weird use of `set` is to remove multiple whitespace
# in date output
set -f
set -- $output
echo "$@"
