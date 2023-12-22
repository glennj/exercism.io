use context essentials2020

include file("binary-search.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun value-in-list-of-one():
  check "finds a value in a list with one element":
    binary-search([list: 6], 6) is 0
  end
end

fun value-in-middle-of-list():
  check "finds a value in the middle of a list":
    binary-search([list: 1, 3, 4, 6, 8, 9, 11], 6) is 3
  end
end


fun value-in-beginning-of-list():
  check "finds a value at the beginning of a list":
    binary-search([list: 1, 3, 4, 6, 8, 9, 11], 1) is 0
  end
end

fun value-at-end-of-list():
  check "finds a value at the end of a list":
    binary-search([list: 1, 3, 4, 6, 8, 9, 11], 11) is 6
  end
end

fun value-in-odd-length-list():
  check "finds a value in a list of odd length":
    binary-search([list: 1, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 634], 144) is 9
  end
end

fun value-in-even-length-list():
  check "finds a value in a list of even length":
    binary-search([list: 1, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377], 21) is 5
  end
end

fun error-value-not-in-list():
  check "identifies that a value is not included in the list":
    binary-search([list: 1, 3, 4, 6, 8, 9, 11], 7) raises "value not in list"
  end
end

fun error-value-too-small():
  check "a value smaller than the list's smallest value is not found":
    binary-search([list: 1, 3, 4, 6, 8, 9, 11], 0) raises "value not in list"
  end
end

fun err-value-too-large():
  check "a value larger than the list's largest value is not found":
    binary-search([list: 1, 3, 4, 6, 8, 9, 11], 13) raises "value not in list"
  end
end

fun err-empty-list():
  check "nothing is found in an empty list":
    binary-search([list: ], 1) raises "value not in list"
  end
end

fun err-bounds-cross():
  check "nothing is found when the left and right bounds cross":
    binary-search([list: 1, 2], 0) raises "value not in list"
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, false) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(value-in-list-of-one, true),
  test(value-in-middle-of-list, true),
  test(value-in-beginning-of-list, true),
  test(value-at-end-of-list, true),
  test(value-in-odd-length-list, true),
  test(value-in-even-length-list, true),
  test(error-value-not-in-list, true),
  test(error-value-too-small, true),
  test(err-value-too-large, true),
  test(err-empty-list, true),
  test(err-bounds-cross, true)
].each(lam(t): when t.active: t.run() end end)
