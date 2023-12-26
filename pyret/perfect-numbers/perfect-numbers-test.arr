use context essentials2020

include file("perfect-numbers.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun smallest-perfect():
  check "Perfect numbers -> Smallest perfect number is classified correctly":
    classify(6) is "perfect"
  end
end

fun medium-perfect-number():
  check "Perfect numbers -> Medium perfect number is classified correctly":
    classify(28) is "perfect"
  end
end

fun large-perfect-number():
  check "Perfect numbers -> Large perfect number is classified correctly":
    classify(33550336) is "perfect"
  end
end

fun smallest-abundant():
  check "Abundant numbers -> Smallest abundant number is classified correctly":
    classify(12) is "abundant"
  end
end

fun medium-abundant():
  check "Abundant numbers -> Medium abundant number is classified correctly":
    classify(30) is "abundant"
  end
end

fun large-abundant():
  check "Abundant numbers -> Large abundant number is classified correctly":
    classify(33550335) is "abundant"
  end
end

fun smallest-prime-deficient():
check "Deficient numbers -> Smallest prime deficient number is classified correctly":
  classify(2) is "deficient"
end
end

fun smallest-non-prime-deficient():
  check "Deficient numbers -> Smallest non-prime deficient number is classified correctly":
    classify(4) is "deficient"
  end
end

fun medium-deficient():
  check "Deficient numbers -> Medium deficient number is classified correctly":
    classify(32) is "deficient"
  end
end

fun large-deficient():
  check "Deficient numbers -> Large deficient number is classified correctly":
    classify(33550337) is "deficient"
  end
end

fun one-is-deficient():
  check "Deficient numbers -> Edge case (no factors other than itself) is classified correctly":
    classify(1) is "deficient"
  end
end

fun reject-zero():
  check "Invalid inputs -> Zero is rejected (as it is not a positive integer)":
    classify(0) raises "Classification is only possible for positive integers."
  end
end

fun reject-negative():
  check "Invalid inputs -> Negative integer is rejected (as it is not a positive integer)":
    classify(-1) raises "Classification is only possible for positive integers."
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, true) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(smallest-perfect, true),
  test(medium-perfect-number, true),
  test(large-perfect-number, true),
  test(smallest-abundant, true),
  test(medium-abundant, true),
  test(large-abundant, true),
  test(smallest-prime-deficient, true),
  test(smallest-non-prime-deficient, true),
  test(medium-deficient, true),
  test(large-deficient, true),
  test(one-is-deficient, true),
  test(reject-zero, true),
  test(reject-negative, true)
].each(lam(t): when t.active: t.run() end end)
