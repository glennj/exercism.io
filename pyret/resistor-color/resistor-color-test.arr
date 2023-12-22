use context essentials2020

include file("resistor-color.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun color-code-black():
  check "Color codes -> Black":
    color-code("black") is 0
  end
end

fun color-code-white():
  check "Color codes -> White":
    color-code("white") is 9
  end
end

fun color-code-orange():
  check "Color codes -> Orange":
    color-code("orange") is 3
  end
end

fun test-colors():
  check "Colors":
    colors() is [list:
      "black",
      "brown",
      "red",
      "orange",
      "yellow",
      "green",
      "blue",
      "violet",
      "grey",
      "white",
      ]
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, false) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(color-code-black, true),
  test(color-code-white, true),
  test(color-code-orange, true),
  test(test-colors, true),
].each(lam(t): when t.active: t.run() end end)
