#!/usr/bin/env gawk -f

function abs(a) {
    return a < 0 ? -a : a
}

function die(msg) {
    print msg > "/dev/stderr"
    exit 1
}

{
    for (i = 1; i <= NF; i++)
        if ($i < 0 || $i > 7)
            die("invalid position")
}

$1 == $3 && $2 == $4 {
    die("invalid board: same position")
}

$1 == $3 || $2 == $4 || abs($1 - $3) == abs($2 - $4) {
    print "true"
    exit
}

{
    print "false"
}
