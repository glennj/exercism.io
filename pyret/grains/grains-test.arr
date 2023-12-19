use context essentials2020

include file("grains.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun on-square-1():
  check "returns the number of grains on the square -> grains on square 1":
    on-square(1) is 1
  end
end

fun on-square-2():
  check "returns the number of grains on the square -> grains on square 2":
    on-square(2) is 2
  end
end

fun on-square-3():
  check "returns the number of grains on the square -> grains on square 3":
    on-square(3) is 4
  end
end

fun on-square-4():
  check "returns the number of grains on the square -> grains on square 4":
    on-square(4) is 8
  end
end

fun on-square-16():
  check "returns the number of grains on the square -> grains on square 16":
    on-square(16) is 32768
  end
end

fun on-square-32():
  check "returns the number of grains on the square -> grains on square 32":
    on-square(32) is 2147483648
  end
end

fun on-square-64():
  check "returns the number of grains on the square -> grains on square 64":
    on-square(64) is 9223372036854775808
  end
end

fun on-square-raises-exception-for-zero():
  check "returns the number of grains on the square -> square 0 raises an exception":
    on-square(0) raises "square must be between 1 and 64"
  end
end

fun on-square-raises-exception-for-negative():
  check "returns the number of grains on the square -> negative square raises an exception":
    on-square(-1) raises "square must be between 1 and 64"
  end
end

fun on-square-raises-exception-for-65():
  check "returns the number of grains on the square -> square greater than 64 raises an exception":
    on-square(65) raises "square must be between 1 and 64"
  end
end

fun total-returns-all-grains():
  check "returns the total number of grains on the board":
    total() is 18446744073709551615
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, false) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(on-square-1, true),
  test(on-square-2, true),
  test(on-square-3, true),
  test(on-square-4, true),
  test(on-square-16, true),
  test(on-square-32, true),
  test(on-square-64, true),
  test(on-square-raises-exception-for-zero, true),
  test(on-square-raises-exception-for-negative, true),
  test(on-square-raises-exception-for-65, true),
  test(total-returns-all-grains, true)
].each(lam(t): when t.active: t.run() end end)
