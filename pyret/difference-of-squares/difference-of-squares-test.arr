use context essentials2020

include file("difference-of-squares.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun square-of-sum-1():
  check "Square the sum of the numbers up to the given number -> square of sum 1":
    square-of-sum(1) is 1
  end
end

fun square-of-sum-5():
  check "Square the sum of the numbers up to the given number -> square of sum 5":
    square-of-sum(5) is 225
  end
end

fun square-of-sum-100():
  check "Square the sum of the numbers up to the given number -> square of sum 100":
    square-of-sum(100) is 25502500
  end
end

fun sum-of-squares-1():
  check "Sum the squares of the numbers up to the given number -> sum of squares 1":
    sum-of-squares(1) is 1
  end
end

fun sum-of-squares-5():
  check "Sum the squares of the numbers up to the given number -> sum of squares 5":
    sum-of-squares(5) is 55
  end
end

fun sum-of-squares-100():
  check "Sum the squares of the numbers up to the given number -> sum of squares 100":
    sum-of-squares(100) is 338350
  end
end

fun difference-of-squares-1():
  check "Subtract sum of squares from square of sums -> difference of squares 1":
    difference-of-squares(1) is 0
  end
end

fun difference-of-squares-5():
  check "Subtract sum of squares from square of sums -> difference of squares 5":
    difference-of-squares(5) is 170
  end
end

fun dfference-of-squares-100():
  check "Subtract sum of squares from square of sums -> difference of squares 100":
    difference-of-squares(100) is 25164150
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, false) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(square-of-sum-1, true),
  test(square-of-sum-5, true),
  test(square-of-sum-100, true),
  test(sum-of-squares-1, true),
  test(sum-of-squares-5, true),
  test(sum-of-squares-100, true),
  test(difference-of-squares-1, true),
  test(difference-of-squares-5, true),
  test(dfference-of-squares-100, true)
].each(lam(t): when t.active: t.run() end end)