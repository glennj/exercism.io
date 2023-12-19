use context essentials2020

include file("two-fer.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun no-name-given():
  check "no name given":
    two-fer("") is "One for you, one for me."
  end
end

fun a-name-given():
  check "a name given":
    two-fer("Alice") is "One for Alice, one for me."
  end
end

fun another-name-given():
  check "another name given":
    two-fer("Bob") is "One for Bob, one for me."
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, false) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(no-name-given, true),
  test(a-name-given, true),
  test(another-name-given, true)
].each(lam(t): when t.active: t.run() end end)
