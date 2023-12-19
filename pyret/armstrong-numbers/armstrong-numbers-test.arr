use context essentials2020

include file("armstrong-numbers.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun zero-is-armstrong():
  check "Zero is an Armstrong number":
    is-armstrong-number(0) is true
  end
end

fun single-digit-is-armstrong():
  check "Single-digit numbers are Armstrong numbers":
    is-armstrong-number(5) is true
  end
end

fun two-digit-is-not-armstrong():
  check "There are no two-digit Armstrong numbers":
    is-armstrong-number(10) is false
  end
end

fun three-digit-is-armstrong():
  check "Three-digit number that is an Armstrong number":
    is-armstrong-number(153) is true
  end
end

fun three-digit-is-not-armstrong():
  check "Three-digit number that is not an Armstrong number":
    is-armstrong-number(100) is false
  end
end

fun four-digit-is-armstrong():
  check "Four-digit number that is an Armstrong number":
    is-armstrong-number(9474) is true
  end
end

fun four-digit-is-not-armstrong():
  check "Four-digit number that is not an Armstrong number":
    is-armstrong-number(9475) is false
  end
end

fun seven-digit-is-armstrong():
  check "Seven-digit number that is an Armstrong number":
    is-armstrong-number(9926315) is true
  end
end

fun seven-digit-is-not-armstrong():
  check "Seven-digit number that is not an Armstrong number":
    is-armstrong-number(9926314) is false
  end
end

fun number-with-seven-zeroes-is-not-armstrong():
  check "Armstrong number containing seven zeroes":
    is-armstrong-number(186709961001538790100634132976990) is true
  end
end

fun largest-armstrong-number():
  check "The largest and last Armstrong number":
    is-armstrong-number(115132219018763992565095597973971522401) is true
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, false) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(zero-is-armstrong, true),
  test(single-digit-is-armstrong, true),
  test(two-digit-is-not-armstrong, true),
  test(three-digit-is-armstrong, true),
  test(three-digit-is-not-armstrong, true),
  test(four-digit-is-armstrong, true),
  test(four-digit-is-not-armstrong, true),
  test(seven-digit-is-armstrong, true),
  test(seven-digit-is-not-armstrong, true),
  test(number-with-seven-zeroes-is-not-armstrong, true),
  test(largest-armstrong-number, true)
].each(lam(t): when t.active: t.run() end end)