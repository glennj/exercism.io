use context essentials2020

include file("triangle.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun equilateral-all-equal-sides():
  check "equilateral triangle -> all sides are equal":
    equilateral([list: 2, 2, 2]) is true
  end
end

fun equilateral-any-unequal-side():
  check "equilateral triangle -> any side is unequal":
    equilateral([list: 2, 3, 2]) is false
  end
end

fun equilateral-no-equal-sides():
  check "equilateral triangle -> no sides are equal":
    equilateral([list: 5, 4, 6]) is false
  end
end

fun equilateral-all-zero-sides():
  check "equilateral triangle -> all zero sides is not a triangle":
    equilateral([list: 0, 0, 0]) is false
  end
end

fun equilateral-decimal-sides():
  check "equilateral triangle -> sides may be decimals":
    equilateral([list: 0.5, 0.5, 0.5]) is true
  end
end

fun isosceles-second-third-equal-sides():
  check "isosceles triangle -> last two sides are equal":
    isosceles([list: 3, 4, 4]) is true
  end
end

fun isosceles-first-second-equal-sides():
  check "isosceles triangle -> first two sides are equal":
    isosceles([list: 4, 4, 3]) is true
  end
end

fun isosceles-first-third-equal-sides():
  check "isosceles triangle -> first and last sides are equal":
    isosceles([list: 4, 3, 4]) is true
  end
end

fun isosceles-can-be-equilateral():
  check "isosceles triangle -> equilateral triangles are also isosceles":
    isosceles([list: 4, 4, 4]) is true
  end
end

fun isosceles-no-equal-sides():
  check "isosceles triangle -> no sides are equal":
    isosceles([list: 2, 3, 4]) is false
  end
end

fun isosceles-triangle-inequality-first():
  check "isosceles triangle -> first triangle inequality violation":
    isosceles([list: 1, 1, 3]) is false
  end
end

fun isosceles-triangle-inequality-second():
  check "isosceles triangle -> second triangle inequality violation":
    isosceles([list: 1, 3, 1]) is false
  end
end

fun isosceles-triangle-inequality-third():
  check "isosceles triangle -> third triangle inequality violation":
    isosceles([list: 3, 1, 1]) is false
  end
end

fun isosceles-decimal-sides():
  check "isosceles triangle -> sides may be decimasl":
    isosceles([list: 0.5, 0.4, 0.5]) is true
  end
end

fun scalene-no-equal-sides():
  check "scalene triangle -> no sides are equal":
    scalene([list: 5, 4, 6]) is true
  end
end

fun scalene-all-equal-sides():
  check "scalene triangle -> all sides are equal":
    scalene([list: 4, 4, 4]) is false
  end
end

fun scalene-first-second-equal-sides():
  check "scalene triangle -> first and second sides are equal":
    scalene([list: 4, 4, 3]) is false
  end
end

fun scalene-first-third-equal-sides():
  check "scalene triangle -> first and third sides are equal":
    scalene([list: 3, 4, 3]) is false
  end
end

fun scalene-second-third-equal-sides():
  check "scalene triangle -> second and third sides are equal":
    scalene([list: 4, 3, 3]) is false
  end
end

fun scalene-triangle-inequality():
  check "scalene triangle -> may not violate triangle inequality":
    scalene([list: 7, 3, 2]) is false
  end
end

fun scalene-decimal-sides():
  check "scalene triangle -> sides may be decimals":
    scalene([list: 0.5, 0.4, 0.6]) is true
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, false) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(equilateral-all-equal-sides, true),
  test(equilateral-any-unequal-side, true),
  test(equilateral-no-equal-sides, true),
  test(equilateral-all-zero-sides, true),
  test(equilateral-decimal-sides, true),
  test(isosceles-second-third-equal-sides, true),
  test(isosceles-first-second-equal-sides, true),
  test(isosceles-first-third-equal-sides, true),
  test(isosceles-can-be-equilateral, true),
  test(isosceles-no-equal-sides, true),
  test(isosceles-triangle-inequality-first, true),
  test(isosceles-triangle-inequality-second, true),
  test(isosceles-triangle-inequality-third, true),
  test(isosceles-decimal-sides, true),
  test(scalene-no-equal-sides, true),
  test(scalene-all-equal-sides, true),
  test(scalene-first-second-equal-sides, true),
  test(scalene-first-third-equal-sides, true),
  test(scalene-second-third-equal-sides, true),
  test(scalene-triangle-inequality, true),
  test(scalene-decimal-sides, true)
].each(lam(t): when t.active: t.run() end end)
