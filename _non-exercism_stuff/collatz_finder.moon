#!/usr/bin/env moon

limit = 100 * 1000 * 1000
print "limit = #{limit}"

isPowerOf2 = (n) -> n == 2 ^ math.log(n, 2)

for n = 2, limit
    m = n
    steps = 0
    found = false
    while steps < 10000 and m != 1
        steps += 1
        m = if m % 2 == 0 then m // 2 else 3 * m + 1
        if m < n or isPowerOf2 m
            found = true -- already seen it!
            break
    assert found, "Not a collatz number: #{n} in steps: #{steps} (#{m})"

print "2 up to #{limit} are collatz numbers"
