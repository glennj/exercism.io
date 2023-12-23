use context essentials2020

include file("matrix.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun extract-row-from-one-row-matrix():
  check "extract row from one number matrix":
    m = matrix("1")
    m.row(1) is [list: 1]
  end
end

fun extract-row-from-two-row-matrix():
  check "can extract row":
    m = matrix("1 2\n3 4")
    m.row(2) is [list: 3, 4]
  end
end

fun extract-row-from-variable-number-width-matrix():
  check "extract row where numbers have different widths":
    m = matrix("1 2\n10 20")
    m.row(2) is [list: 10, 20]
  end
end

fun extract-row-from-non-square-matrix():
  check "can extract row from non-square matrix with no corresponding column":
    m = matrix("1 2 3\n4 5 6\n7 8 9\n8 7 6")
    m.row(4) is [list: 8, 7, 6]
  end
end


fun extract-column-from-one-column-matrix():
  check "extract column from one number matrix":
    m = matrix("1")
    m.column(1) is [list: 1]
  end
end

fun extract-column-from-three-column-matrix():
  check "can extract column":
    m = matrix("1 2 3\n4 5 6\n7 8 9")
    m.column(3) is [list: 3, 6, 9]
  end
end

fun extract-column-from-non-square-matrix():
  check "can extract column from non-square matrix with no corresponding row":
    m = matrix("1 2 3 4\n5 6 7 8\n9 8 7 6")
    m.column(4) is [list: 4, 8, 6]
  end
end

fun extract-column-from-variable-number-width-matrix():
  check "extract column where numbers have different widths":
    m = matrix("89 1903 3\n18 3 1\n9 4 800")
    m.column(2) is [list: 1903, 3, 4]
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, false) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(extract-row-from-one-row-matrix, true),
  test(extract-row-from-two-row-matrix, true),
  test(extract-row-from-variable-number-width-matrix, true),
  test(extract-row-from-non-square-matrix, true),
  test(extract-column-from-one-column-matrix, true),
  test(extract-column-from-three-column-matrix, true),
  test(extract-column-from-non-square-matrix, true),
  test(extract-column-from-variable-number-width-matrix, true)
].each(lam(t): when t.active: t.run() end end)
