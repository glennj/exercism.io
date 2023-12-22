use context essentials2020

include file("square-root.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun root-of-one():
  check "root of 1":
    square-root(1) is 1
  end
end

fun root-of-four():
  check "root of 4":
    square-root(4) is 2
  end
end

fun root-of-twenty-five():
  check "root of 25":
    square-root(25) is 5
  end
end

fun root-of-eighty-one():
  check "root of 81":
    square-root(81) is 9
  end
end

fun root-of-one-hundred-ninety-six():
  check "root of 196":
    square-root(196) is 14
  end
end

fun root-of-sixty-five-thousand-twenty-five():
  check "root of 65025":
    square-root(65025) is 255
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, false) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(root-of-one, true),
  test(root-of-four, true),
  test(root-of-twenty-five, true),
  test(root-of-eighty-one, true),
  test(root-of-one-hundred-ninety-six, true),
  test(root-of-sixty-five-thousand-twenty-five, true)
].each(lam(t): when t.active: t.run() end end)
