use context essentials2020

include file("line-up.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#


fun format-4():
  check "format smallest non-exceptional ordinal numeral 4":
    format-message("Gianna", 4) is "Gianna, you are the 4th customer we serve today. Thank you!"
  end
end

fun format-9():
  check "format greatest single digit non-exceptional ordinal numeral 9":
    format-message("Maarten", 9) is "Maarten, you are the 9th customer we serve today. Thank you!"
  end
end

fun format-5():
  check "format non-exceptional ordinal numeral 5":
    format-message("Petronila", 5) is "Petronila, you are the 5th customer we serve today. Thank you!"
  end
end

fun format-6():
  check "format non-exceptional ordinal numeral 6":
    format-message("Attakullakulla", 6) is "Attakullakulla, you are the 6th customer we serve today. Thank you!"
  end
end

fun format-7():
  check "format non-exceptional ordinal numeral 7":
    format-message("Kate", 7) is "Kate, you are the 7th customer we serve today. Thank you!"
  end
end

fun format-8():
  check "format non-exceptional ordinal numeral 8":
    format-message("Maximiliano", 8) is "Maximiliano, you are the 8th customer we serve today. Thank you!"
  end
end

fun format-1():
  check "format exceptional ordinal numeral 1":
    format-message("Mary", 1) is "Mary, you are the 1st customer we serve today. Thank you!"
  end
end

fun format-2():
  check "format exceptional ordinal numeral 2":
    format-message("Haruto", 2) is "Haruto, you are the 2nd customer we serve today. Thank you!"
  end
end

fun format-3():
  check "format exceptional ordinal numeral 3":
    format-message("Henriette", 3) is "Henriette, you are the 3rd customer we serve today. Thank you!"
  end
end

fun format-10():
  check "format smallest two digit non-exceptional ordinal numeral 10":
    format-message("Alvarez", 10) is "Alvarez, you are the 10th customer we serve today. Thank you!"
  end
end

fun format-11():
  check "format non-exceptional ordinal numeral 11":
    format-message("Jacqueline", 11) is "Jacqueline, you are the 11th customer we serve today. Thank you!"
  end
end

fun format-12():
  check "format non-exceptional ordinal numeral 12":
    format-message("Juan", 12) is "Juan, you are the 12th customer we serve today. Thank you!"
  end
end

fun format-13():
  check "format non-exceptional ordinal numeral 13":
    format-message("Patricia", 13) is "Patricia, you are the 13th customer we serve today. Thank you!"
  end
end

fun format-21():
  check "format exceptional ordinal numeral 21":
    format-message("Washi", 21) is "Washi, you are the 21st customer we serve today. Thank you!"
  end
end

fun format-62():
  check "format exceptional ordinal numeral 62":
    format-message("Nayra", 62) is "Nayra, you are the 62nd customer we serve today. Thank you!"
  end
end

fun format-100():
  check "format exceptional ordinal numeral 100":
    format-message("John", 100) is "John, you are the 100th customer we serve today. Thank you!"
  end
end

fun format-101():
  check "format exceptional ordinal numeral 101":
    format-message("Zeinab", 101) is "Zeinab, you are the 101st customer we serve today. Thank you!"
  end
end

fun format-112():
  check "format non-exceptional ordinal numeral 112":
    format-message("Knud", 112) is "Knud, you are the 112th customer we serve today. Thank you!"
  end
end

fun format-123():
  check "format exceptional ordinal numeral 123":
    format-message("Yma", 123) is "Yma, you are the 123rd customer we serve today. Thank you!"
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, true) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(format-4, true),
  test(format-9, true),
  test(format-5, true),
  test(format-6, true),
  test(format-7, true),
  test(format-8, true),
  test(format-1, true),
  test(format-2, true),
  test(format-3, true),
  test(format-10, true),
  test(format-11, true),
  test(format-12, true),
  test(format-13, true),
  test(format-21, true),
  test(format-62, true),
  test(format-100, true),
  test(format-101, true),
  test(format-112, true),
  test(format-123, true)
].each(lam(t): when t.active: t.run() end end)
