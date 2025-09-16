#!gawk

function die(msg) {
    print msg > "/dev/stderr"
    _die_flag = 1
    exit 1
}

function assert(cond, msg) {if (!cond) die(msg)}

END {
    if (_die_flag)
        exit 1
}
