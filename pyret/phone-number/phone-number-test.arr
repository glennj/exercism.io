use context essentials2020

include file("phone-number.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun clean-number():
  check "cleans the number":
    clean("(223) 456-7890") is "2234567890"
  end
end

fun clean-number-with-dots():
  check "cleans numbers with dots":
    clean("223.456.7890") is "2234567890"
  end
end

fun clean-number-with-spaces():
  check "cleans numbers with multiple spaces":
    clean("223 456   7890   ") is "2234567890"
  end
end

fun invalid-nine-digits():
  check "invalid when 9 digits":
    clean("123456789") raises "must not be fewer than 10 digits"
  end
end

fun invalid-eleven-digits-starting-with-two():
  check "invalid when 11 digits does not start with a 1":
    clean("22234567890") raises "11 digits must start with 1"
  end
end

fun valid-eleven-digits-starting-with-one():
  check "valid when 11 digits and starting with 1":
    clean("12234567890") is "2234567890"
  end
end

fun valid-eleven-digits-starting-with-one-and-punctuation():
  check "valid when 11 digits and starting with 1 even with punctuation":
    clean("+1 (223) 456-7890") is "2234567890"
  end
end

fun invalid-more-than-eleven-digits():
  check "invalid when more than 11 digits":
    clean("321234567890") raises "must not be greater than 11 digits"
  end
end

fun invalid-with-letters():
  check "invalid with letters":
    clean("523-abc-7890") raises "letters not permitted"
  end
end

fun invalid-with-punctuation():
  check "invalid with punctuations":
    clean("523-@:!-7890") raises "punctuations not permitted"
  end
end

fun invalid-area-code-starts-with-zero():
  check "invalid if area code starts with 0":
    clean("(023) 456-7890") raises "area code cannot start with zero"
  end
end

fun invalid-area-code-starts-with-one():
  check "invalid if area code starts with 1":
    clean("(123) 456-7890") raises "area code cannot start with one"
  end
end

fun invalid-exchange-code-starts-with-zero():
  check "invalid if exchange code starts with 0":
    clean("(223) 056-7890") raises "exchange code cannot start with zero"
  end
end

fun invalid-exchange-code-starts-with-one():
  check "invalid if exchange code starts with 1":
    clean("(223) 156-7890") raises "exchange code cannot start with one"
  end
end

fun invalid-eleven-digits-and-area-code-starts-with-zero():
  check "invalid if area code starts with 0 on valid 11-digit number":
    clean("1 (023) 456-7890") raises "area code cannot start with zero"
  end
end

fun invalid-eleven-digits-and-area-code-starts-with-one():
  check "invalid if area code starts with 1 on valid 11-digit number":
    clean("1 (123) 456-7890") raises  "area code cannot start with one"
  end
end

fun invalid-eleven-digits-and-exchange-code-starts-with-zero():
  check "invalid if exchange code starts with 0 on valid 11-digit number":
    clean("1 (223) 056-7890") raises "exchange code cannot start with zero"
  end
end

fun invalid-eleven-digits-and-exchange-code-starts-with-one():
  check "invalid if exchange code starts with 1 on valid 11-digit number":
    clean("1 (223) 156-7890") raises "exchange code cannot start with one"
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, false) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(clean-number, true),
  test(clean-number-with-dots, true),
  test(clean-number-with-spaces, true),
  test(invalid-nine-digits, true),
  test(invalid-eleven-digits-starting-with-two, true),
  test(valid-eleven-digits-starting-with-one, true),
  test(valid-eleven-digits-starting-with-one-and-punctuation, true),
  test(invalid-more-than-eleven-digits, true),
  test(invalid-with-letters, true),
  test(invalid-with-punctuation, true),
  test(invalid-area-code-starts-with-zero, true),
  test(invalid-area-code-starts-with-one, true),
  test(invalid-exchange-code-starts-with-zero, true),
  test(invalid-exchange-code-starts-with-one, true),
  test(invalid-eleven-digits-and-area-code-starts-with-zero, true ),
  test(invalid-eleven-digits-and-area-code-starts-with-one, true),
  test(invalid-eleven-digits-and-exchange-code-starts-with-zero, true),
  test(invalid-eleven-digits-and-exchange-code-starts-with-one, true)
].each(lam(t): when t.active: t.run() end end)
