use context essentials2020

include file("leap.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun not-divisible-by-4():
  check "year not divisible by 4 in common year":
    leap(2015) is false
  end
end

fun divisible-by-2-not-4():
  check "year divisible by 2, not divisible by 4 in common year":
    leap(1970) is false
  end
end

fun divisible-by-4-not-100():
  check "year divisible by 4, not divisible by 100 in leap year":
    leap(1996) is true
  end
end

fun divisible-by-4-and-5():
  check "year divisible by 4 and 5 is still a leap year":
    leap(1960) is true
  end
end

fun divisible-by-100-not-400():
  check "year divisible by 100, not divisible by 400 in common year":
    leap(2100) is false
  end
end

fun divisible-by-100-not-3():
  check "year divisible by 100 but not by 3 is still not a leap year":
    leap(1900) is false
  end
end

fun divisible-by-400():
  check "year divisible by 400 is leap year":
    leap(2000) is true
  end
end

fun divisible-by-400-not-125():
  check "year divisible by 400 but not by 125 is still a leap year":
    leap(2400) is true
  end
end

fun divisible-by-200-not-400():
  check "year divisible by 200, not divisible by 400 in common year":
    leap(1800) is false
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, false) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(not-divisible-by-4, true),
  test(divisible-by-2-not-4, true),
  test(divisible-by-4-not-100, true),
  test(divisible-by-4-and-5, true),
  test(divisible-by-100-not-400, true),
  test(divisible-by-100-not-3, true),
  test(divisible-by-400, true),
  test(divisible-by-400-not-125, true),
  test(divisible-by-200-not-400, true)
].each(lam(t): when t.active: t.run() end end)
