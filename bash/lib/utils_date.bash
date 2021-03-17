#!/usr/bin/env bash

# Provide a (more) portable front-end to date parsing and
# formatting.
#
# I'll check for the existance of common date utilities:
#   GNU date
#   BSD date (for MacOS)
#   perl Time::Piece
#

# this is a library, don't want to exit
error() {
    echo "$*" >&2
    return 1
}

############################################################
date::determineCapabilities() {
    local exe

    # Using functions to avoid needing a global array.
    # They also help keep things in the same namespace.
    date::gnu() { false; }
    date::bsd() { false; }
    date::perl() { false; }

    if exe=$(command -v date); then
        if { "$exe" --version | grep -q 'GNU coreutils'; } 2> /dev/null; then
            date::gnu() { true; }

        elif { strings "$exe" | grep -q 'PROGRAM:date.*PROJECT:shell_cmds'; } 2> /dev/null; then
            # Probably MacOS date.
            # This BSD-derived date is less flexible about date parsing.
            date::bsd() { true; }

        else
            : # What `date` is this??
        fi

        # ugh. shrug.
        eval "date::exe() { \"$exe\" \"\$@\"; }"
    fi
    if { command -v perl && perl -MTime::Piece -e 1; } &> /dev/null; then
        date::perl() { true; }
    fi

    # uncomment for testing
    #date::gnu() { false; }
    #date::bsd() { true; }; date::exe() { /bin/date "$@"; }
    #date::bsd() { false; }
    #date::perl() { false; }
}

############################################################
# Return the current time in epoch seconds
#
date::now() {
    # bash v5 has the EPOCHSECONDS builtin variable,
    # unless it has been unset
    if  ((10 * BASH_VERSINFO[0] + BASH_VERSINFO[1] >= 50)) &&
        [[ -n ${EPOCHSECONDS+set} ]]
    then
        echo "$EPOCHSECONDS"

    elif date::gnu || date::bsd; then
        date::exe +%s

    elif date::perl; then
        perl -le 'print time'

    fi
}

############################################################
# Parse a datetime string, return the time in epoch seconds.
# usage: date::parse [-option] timestamp
# options:
#   -u     => UTC timezone
#   -f fmt => timestamp format (default: YYYY-mm-ddTHH:MM:SS)
#
date::parse() {
    local utc=false
    local fmt='%Y-%m-%dT%H:%M:%S'
    local opt OPTIND OPTARG
    while getopts :f:u opt; do
        case $opt in
            u) utc=true ;;
            f) fmt=$OPTARG ;;
            *) : ;;
        esac
    done
    shift $((OPTIND - 1))

    [[ $1 ]] || error "usage: ${FUNCNAME[0]} [-u] [-f format] timestamp"

    local timestamp=$1
    local -a args

    if date::gnu; then
        # ignore the given format: GNU date uses free-form parsing
        # https://www.gnu.org/software/coreutils/manual/html_node/Date-input-formats.html
        $utc && args+=("--utc")
        args+=("--date=$timestamp")
        args+=("+%s")
        date::exe "${args[@]}"

    elif date::bsd; then
        # Note, the input datetime string _must_ match the format!
        $utc && args+=("-u")
        args+=("-j")
        args+=("-f" "$fmt" "$timestamp")
        args+=("+%s")
        date::exe "${args[@]}"

    elif date::perl; then
        perl -MTime::Piece -sle '
            # necessary to set TZ env var?
            $ENV{TZ} = "UTC" if $utc eq "true";
            print Time::Piece->strptime($input, $fmt)->epoch;
        ' -- -input="$timestamp" -fmt="$fmt" -utc=$utc

    else
        error "Don't know how to parse a datetime string"
    fi
}

############################################################
# Format an time (in epoch seconds) using the given format
# usage: date::format [-option] timestamp
# options:
#   -u     => UTC timezone
#   -f fmt => output format (default: YYYY-mm-ddTHH:MM:SS)
#
date::format() {
    local utc=false
    local fmt='%Y-%m-%dT%H:%M:%S'
    local opt OPTIND OPTARG
    while getopts :f:u opt; do
        case $opt in
            u) utc=true ;;
            f) fmt=$OPTARG ;;
            *) : ;;
        esac
    done
    shift $((OPTIND - 1))

    [[ $1 ]] || error "usage: ${FUNCNAME[0]} [-f format] [-u] timestamp"

    local -i timestamp=$1
    local -a args

    if ((10 * BASH_VERSINFO[0] + BASH_VERSINFO[1] >= 43)); then
        # version 4.3 printf has a "%(format)T" date formatter
        if $utc; then
            TZ=UTC printf "%($fmt)T\n" "$1"
        else
            printf "%($fmt)T\n" "$1"
        fi

    elif date::gnu; then
        $utc && args+=("--utc")
        args+=("--date=@${timestamp}")
        args+=("+$fmt")
        date::exe "${args[@]}"

    elif date::bsd; then
        $utc && args+=("-u")
        args+=("-j")
        args+=("-f" "%s" "$timestamp")
        args+=("+$fmt")
        date::exe "${args[@]}"

    elif date::perl; then
        perl -MTime::Piece -sle '
            my $instant = $utc eq "true"
                ? gmtime($time)
                : localtime($time);
            print $instant->strftime($fmt);
        ' -- -time="$timestamp" -fmt="$fmt" -utc=$utc
    fi
}

date::determineCapabilities
