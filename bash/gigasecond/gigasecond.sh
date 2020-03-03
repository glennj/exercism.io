#!/usr/bin/env bash

source ../lib/utils.bash    # `die`

# some globals
declare -r gigasecond=1000000000
declare -r format="%Y-%m-%dT%H:%M:%S"

gnuDate=false
bsdDate=false
perlDate=false
bashDateOutput=false

main() {
    if (( $# != 1 )); then
        die "usage: ${0##*/} time-spec"
    fi

    determineDateCapabilities
    epoch=$( parseTimestamp "$1" ) || die "Invalid time-spec '$1'"
    outputTimestamp $(( epoch + gigasecond ))
}

# try to make this as platform-agnostic as possible
determineDateCapabilities() {
    if { date --version | grep -q "GNU coreutils"; } 2>/dev/null; then
        gnuDate=true 
    fi
    if [[ $(what "$(type -P date)") == *"PROGRAM:date"*"PROJECT:shell_cmds"* ]]; then
        # Possibly MacOS date.
        # This BSD-derived date is less flexible about date parsing.
        bsdDate=true 
    fi
    if perl -MTime::Piece -e 1 2>/dev/null; then
        perlDate=true 
    fi
    if (( ${BASH_VERSINFO[0]}${BASH_VERSINFO[1]} >= 43 )); then
        # version 4.3 printf has a "%(format)T" date formatter
        bashDateOutput=true
    fi
    $gnuDate || $bsdDate || $perlDate || 
        die "Don't know how to parse timestamp"
}

# this function will return with the called program's
# exit status -- useful to detect invalid input.
parseTimestamp() {
    if $gnuDate; then
        date --utc --date="$1" "+%s" 2>/dev/null

    elif $bsdDate; then
        _epoch() { 
            date -j -u -f "$format" "$1" "+%s" 2>/dev/null
        }
        _epoch "$1" || _epoch "${1}T00:00:00"

    elif $perlDate; then
        perl -MTime::Piece -sE '
            say Time::Piece->strptime($input, $fmt)->epoch;
        ' -- -input="$1" -fmt="$format" 2>/dev/null

    fi
}

outputTimestamp() {
    if $bashDateOutput; then
        TZ=UTC printf "%($format)T\n" "$1"

    elif $gnuDate; then
        date --utc --date="@$1" "+$format"

    elif $bsdDate; then
        date -j -u -f "%s" "$1" "+$format"

    elif $perlDate; then
        perl -MTime::Piece -sE '
            my $instant = gmtime($time);
            say $instant->strftime($fmt);
        ' -- -time="$1" -fmt="$format"
    fi
}

main "$@"
