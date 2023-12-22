use context essentials2020

include file("darts.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun missed-target():
  check "Missed target":
    score(-9, 9) is 0
  end
end

fun on-outer-circle():
  check "On the outer circle":
    score(0, 10) is 1
  end
end

fun on-middle-circle():
  check "On the middle circle":
    score(-5, 0) is 5
  end
end

fun on-inner-circle():
  check "On the inner circle":
    score(0, -1) is 10
  end
end

fun at-center():
  check "Exactly on center":
    score(0, 0) is 10
  end
end

fun near-center():
  check "Near the center":
    score(-0.1, -0.1) is 10
  end
end

fun just-within-inner-circle():
  check "Just within the inner circle":
    score(0.7, 0.7) is 10
  end
end

fun just-outside-inner-circle():
  check "Just outside the inner circle":
    score(0.8, -0.8) is 5
  end
end

fun just-within-middle-circle():
  check "Just within the middle circle":
    score(-3.5, 3.5) is 5
  end
end

fun just-outside-middle-circle():
  check "Just outside the middle circle":
    score(-3.6, 3.6) is 1
  end
end

fun just-within-outer-circle():
  check "Just within the outer circle":
    score(-7.0, 7.0) is 1
  end
end

fun just-outside-outer-circle():
  check "Just outside the outer circle":
    score(7.1, -7.1) is 0
  end
end

fun asymmetric-position():
  check "Asymmetric position between the inner and middle circles":
    score(0.5, -4) is 5
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, false) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(missed-target, true),
  test(on-outer-circle, true),
  test(on-middle-circle, true),
  test(on-inner-circle, true),
  test(at-center, true),
  test(near-center, true),
  test(just-within-inner-circle, true),
  test(just-outside-inner-circle, true),
  test(just-within-middle-circle, true),
  test(just-outside-middle-circle, true),
  test(just-within-outer-circle, true),
  test(just-outside-outer-circle, true),
  test(asymmetric-position, true)
].each(lam(t): when t.active: t.run() end end)
