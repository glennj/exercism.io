use context essentials2020

include file("series.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun slices-of-one-from-one():
  check "slices of one from one":
    slices("1", 1) is [list: "1"]
  end
end

fun slices-of-one-from-two():
  check "slices of one from two":
    slices("12", 1) is [list: "1", "2"]
  end
end

fun slices-of-two():
  check "slices of two":
    slices("35", 2) is [list: "35"]
  end
end

fun slices-of-two-overlap():
  check "slices of two overlap":
    slices("9142", 2) is [list: "91", "14", "42"]
  end
end

fun slices-can-include-duplicates():
  check "slices can include duplicates":
    slices("777777", 3) is [list: "777", "777", "777", "777"]
  end
end

fun slices-of-a-long-series():
  check "slices of a long series":
    slices("918493904243", 5) is [list: "91849", "18493", "84939", "49390", "93904", "39042", "90424", "04243"]
  end
end

fun slice-length-too-large():
  check "slice length is too large":
    slices("12345", 6) raises "slice length cannot be greater than series length"
  end
end

fun slice-length-way-too-large():
  check "slice length is way too large":
    slices("12345", 42) raises "slice length cannot be greater than series length"
  end
end

fun slice-length-can-not-be-zero():
  check "slice length cannot be zero":
    slices("12345", 0) raises "slice length cannot be zero"
  end
end

fun slice-length-can-not-be-negative():
  check "slice length cannot be negative":
    slices("12345", -1) raises "slice length cannot be negative"
  end
end

fun empty-series-is-invalid():
  check "empty series is invalid":
    slices("", 1) raises "series cannot be empty"
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, false) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(slices-of-one-from-one, true),
  test(slices-of-one-from-two, true),
  test(slices-of-two, true),
  test(slices-of-two-overlap, true),
  test(slices-can-include-duplicates, true),
  test(slices-of-a-long-series, true),
  test(slice-length-too-large, true),
  test(slice-length-way-too-large, true),
  test(slice-length-can-not-be-zero, true),
  test(slice-length-can-not-be-negative, true),
  test(empty-series-is-invalid, true)
].each(lam(t): when t.active: t.run() end end)
