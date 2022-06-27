#!/usr/bin/env gawk -f
# inspired by this lovely recursive solution
# https://exercism.org/tracks/javascript/exercises/say/solutions/515ab00bc90f46b0bde3732d9317a46b

BEGIN {
    nums = "one two three four five six seven eight nine ten eleven twelve thirteen fourteen fifteen sixteen seventeen eightteen nineteen"
    split(nums, small, " ")
    small[0] = "zero"
    tens[20] = "twenty"; tens[30] = "thirty"; tens[40] = "forty"; tens[50] = "fifty"
    tens[60] = "sixty"; tens[70] = "seventy"; tens[80] = "eighty"; tens[90] = "ninety"
}

{ print say($1) }

function say(n) {
    assert(0 <= n && n < 1e12, "input out of range")

    if (n < 100)  return say_small(n)
    if (n < 1e3)  return say_compound(n, 100, "hundred")
    if (n < 1e6)  return say_compound(n, 1e3, "thousand")
    if (n < 1e9)  return say_compound(n, 1e6, "million")
                  return say_compound(n, 1e9, "billion")
}

function say_small(n,    r) {
    if (n in small) return small[n]
    if (n in tens)  return tens[n]
    r = n % 10;     return tens[n - r] "-" small[r]
}

function say_compound(n, base, unit,    r) {
    r = n % base
    return say((n - r) / base) " " unit (r == 0 ? "" : " " say(r))
}

function assert (cond, msg) {
    if (cond) return
    print msg > "/dev/stderr"
    exit 1
}
