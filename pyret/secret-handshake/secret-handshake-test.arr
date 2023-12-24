use context essentials2020

include file("secret-handshake.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun wink():
  check "wink for 1":
    commands("00001") is [list: "wink"]
  end
end

fun double-blink():
  check "double blink for 10":
    commands("00010") is [list: "double blink"]
  end
end

fun close-your-eyes():
  check "close your eyes for 100":
    commands("00100") is [list: "close your eyes"]
  end
end

fun jump():
  check "jump for 1000":
    commands("01000") is [list: "jump"]
  end
end

fun combine-two-actions():
  check "combine two actions":
    commands("00011") is [list: "wink", "double blink"]
  end
end

fun reverse-two-actions():
  check "reverse two actions":
    commands("10011") is [list: "double blink", "wink"]
  end
end

fun reversed-action-is-same():
  check "reversing one action gives the same action":
    commands("11000") is [list: "jump"]
  end
end

fun reversed-inaction-is-same():
  check "reversing no actions still gives no actions":
    commands("10000") is [list: ]
  end
end

fun all-possible-actions():
  check "all possible actions":
    commands("01111") is [list: "wink", "double blink", "close your eyes", "jump"]
  end
end

fun all-possible-actions-reversed():
  check "reverse all possible actions":
    commands("11111") is [list: "jump", "close your eyes", "double blink", "wink"]
  end
end

fun do-nothing():
  check "do nothing for zero":
    commands("00000") is [list: ]
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, true) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(wink, true),
  test(double-blink, true),
  test(close-your-eyes, true),
  test(jump, true),
  test(combine-two-actions, true),
  test(reverse-two-actions, true),
  test(reversed-action-is-same, true),
  test(reversed-inaction-is-same, true),
  test(all-possible-actions, true),
  test(all-possible-actions-reversed, true),
  test(do-nothing, true)
].each(lam(t): when t.active: t.run() end end)
