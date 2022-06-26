#!/usr/bin/env gawk -f

$0 == "total" {
    sum = 0
    for (i = 1; i <= 64; i++)
        sum += grains(i)
    print sum
    next
}
{
    assert(1 <= $0 && $0 <= 64, "square must be between 1 and 64")
    print grains($0)
    next
}

function grains(square) {
    return 2^(square - 1)
}
function assert(cond, msg) {
    if (!cond) {
        print msg > "/dev/stderr"
        exit 1
    }
}