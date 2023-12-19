use context essentials2020

include file("collatz-conjecture.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun zero-steps-for-one():
  check "zero steps for one":
      steps(1) is 0
  end
end

fun divide-if-even():
  check "divide if even":
    steps(16) is 4
  end
end

fun even-and-odd-steps():
  check "even and odd steps":
    steps(12) is 9
  end
end

fun large-number-of-even-and-odd-steps():
  check "large number of even and odd steps":
    steps(1000000) is 152
  end
end

fun zero-is-an-error():
  check "zero is an error":
    steps(0) raises "Only positive numbers are allowed"
  end
end

fun negative-is-an-error():
  check "negative value is an error":
    steps(-15) raises "Only positive numbers are allowed"
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, false) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(zero-steps-for-one, true),
  test(divide-if-even, true),
  test(even-and-odd-steps, true),
  test(large-number-of-even-and-odd-steps, true),
  test(zero-is-an-error, true),
  test(negative-is-an-error, true)
].each(lam(t): when t.active: t.run() end end)
