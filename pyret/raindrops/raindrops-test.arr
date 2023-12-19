use context essentials2020

include file("raindrops.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun convert-1():
  check "the sound for 1 is 1":
    convert(1) is "1"
  end
end

fun convert-3():
  check "the sound for 3 is Pling":
    convert(3) is "Pling"
  end
end

fun convert-5():
  check "the sound for 5 is Plang":
    convert(5) is "Plang"
  end
end

fun convert-7():
  check "the sound for 7 is Plong":
    convert(7) is "Plong"
  end
end

fun convert-6():
  check "the sound for 6 is Pling as it has a factor 3":
    convert(6) is "Pling"
  end
end

fun convert-8():
  check "2 to the power 3 does not make a raindrop sound as 3 is the exponent not the base":
    convert(8) is "8"
  end
end

fun convert-9():
  check "the sound for 9 is Pling as it has a factor 3":
    convert(9) is "Pling"
  end
end

fun convert-10():
  check "the sound for 10 is Plang as it has a factor 5":
    convert(10) is "Plang"
  end
end

fun convert-14():
  check "the sound for 14 is Plong as it has a factor of 7":
    convert(14) is "Plong"
  end
end

fun convert-15():
  check "the sound for 15 is PlingPlang as it has factors 3 and 5":
    convert(15) is "PlingPlang"
  end
end

fun convert-21():
  check "the sound for 21 is PlingPlong as it has factors 3 and 7":
    convert(21) is "PlingPlong"
  end
end

fun convert-25():
  check "the sound for 25 is Plang as it has a factor 5":
    convert(25) is "Plang"
  end
end

fun convert-27():
  check "the sound for 27 is Pling as it has a factor 3":
    convert(27) is "Pling"
  end
end

fun convert-35():
  check "the sound for 35 is PlangPlong as it has factors 5 and 7":
    convert(35) is "PlangPlong"
  end
end

fun convert-49():
  check "the sound for 49 is Plong as it has a factor 7":
    convert(49) is "Plong"
  end
end

fun convert-52():
  check "the sound for 52 is 52":
    convert(52) is "52"
  end
end

fun convert-105():
  check "the sound for 105 is PlingPlangPlong as it has factors 3, 5 and 7":
    convert(105) is "PlingPlangPlong"
  end
end

fun convert-3125():
  check "the sound for 3125 is Plang as it has a factor 5":
    convert(3125) is "Plang"
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, false) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(convert-1, true),
  test(convert-3, true),
  test(convert-5, true),
  test(convert-7, true),
  test(convert-6, true),
  test(convert-8, true),
  test(convert-9, true),
  test(convert-10, true),
  test(convert-14, true),
  test(convert-15, true),
  test(convert-21, true),
  test(convert-25, true),
  test(convert-27, true),
  test(convert-35, true),
  test(convert-49, true),
  test(convert-52, true),
  test(convert-105, true)
].each(lam(t): when t.active: t.run() end end)
