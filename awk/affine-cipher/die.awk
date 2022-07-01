#!gawk

function die(msg) {
    print msg > "/dev/stderr"
    exit 1
}

function assert(cond, msg) {if (!cond) die(msg)}
