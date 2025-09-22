#!/usr/bin/env gawk -f

BEGIN {
    debug("starting ...")
    print "Hello, World!"
    debug("... finished")
}

function debug(msg) {print msg > "/dev/fd/3"}
