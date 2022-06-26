#!/usr/bin/env gawk -f

# These variables are initialized on the command line (using '-v'):
# - flags
# - pattern

BEGIN {
    opts["n"] = opts["l"] = opts["i"] = opts["v"] = opts["x"] = 0
    n = split(flags, f, " ")
    for (i = 1; i <= n; i++) opts[f[i]] = 1

    IGNORECASE = opts["i"]
    multiple_files = ARGC > 2
    matches = 0
}

# match() returns the _number_ of matches, but xor() needs a _boolean_
! xor(!!match($0, pattern, m), opts["v"]) {next}

# check for full-line matching
opts["x"] && xor(m[0] != $0, opts["v"]) {next}

{
    matches++
    if (opts["l"]) {
        print FILENAME
        nextfile
    }
    output = $0
    if (opts["n"])      output = FNR ":" output
    if (multiple_files) output = FILENAME ":" output
    print output
}

END { exit (!matches) }
