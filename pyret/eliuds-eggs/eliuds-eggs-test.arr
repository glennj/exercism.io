use context essentials2020

include file("eliuds-eggs.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun zero-eggs():
  check "0 eggs":
    egg-count(0) is 0
  end
end

fun one-egg():
  check "1 egg":
    egg-count(16) is 1
  end
end

fun four-eggs():
  check "4 eggs":
    egg-count(89) is 4
  end
end

fun thirteen-eggs():
  check "13 eggs":
    egg-count(2000000000) is 13
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, true) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(zero-eggs, true),
  test(one-egg, true),
  test(four-eggs, true),
  test(thirteen-eggs, true)
].each(lam(t): when t.active: t.run() end end)
