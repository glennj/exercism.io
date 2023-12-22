use context essentials2020

include file("resistor-color-duo.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun brown-and-black():
  check "Brown and black":
    color-code([list: "brown", "black"]) is 10
  end
end

fun blue-and-grey():
  check "Blue and grey":
    color-code([list: "blue", "grey"]) is 68
  end
end

fun yellow-and-violet():
  check "Yellow and violet":
    color-code([list: "yellow", "violet"]) is 47
  end
end

fun white-and-red():
  check "White and red":
    color-code([list: "white", "red"]) is 92
  end
end

fun orange-and-orange():
  check  "Orange and orange":
    color-code([list: "orange", "orange"]) is 33
  end
end

fun ignore-additional-colors():
  check "Ignore additional colors":
    color-code([list: "green", "brown", "orange"]) is 51
  end
end

fun black-and-brown():
  check "Black and brown, one-digit":
    color-code([list: "black", "brown"]) is 1
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, false) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(brown-and-black, true),
  test(blue-and-grey, true),
  test(yellow-and-violet, true),
  test(white-and-red, true),
  test(orange-and-orange, true),
  test(ignore-additional-colors, true),
  test(black-and-brown, true)
].each(lam(t): when t.active: t.run() end end)
