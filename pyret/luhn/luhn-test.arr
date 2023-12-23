use context essentials2020

include file("luhn.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun invalid-single-digit():
  check "single digit strings can not be valid":
    is-valid("1") is false
  end
end

fun invalid-single-zero():
  check "a single zero is invalid":
    is-valid("0") is false
  end
end

fun valid-even-if-reversed():
  check "a simple valid SIN that remains valid if reversed":
    is-valid("059") is true
  end
end

fun valid-but-not-reversed():
  check "a simple valid SIN that becomes invalid if reversed":
    is-valid("59") is true
  end
end

fun valid-canadian():
  check "a valid Canadian SIN":
    is-valid("055 444 285") is true
  end
end

fun invalid-canadian():
  check "invalid Canadian SIN":
    is-valid("055 444 286") is false
  end
end

fun invalid-credit-card():
  check "invalid credit card":
    is-valid("8273 1232 7352 0569") is false
  end
end

fun invalid-long-number-even-remainder():
  check "invalid long number with an even remainder":
    is-valid("1 2345 6789 1234 5678 9012") is false
  end
end

fun invalid-long-number-remainder-div-by-five():
  check "invalid long number with a remainder divisible by 5":
    is-valid("1 2345 6789 1234 5678 9013") is false
  end
end

fun valid-number-with-even-number-of-digits():
  check "valid number with an even number of digits":
    is-valid("095 245 88") is true
  end
end

fun valid-number-with-odd-nummber-of-spaces():
  check "valid number with an odd number of spaces":
    is-valid("234 567 891 234") is true
  end
end

fun invalid-number-after-non-digit-added():
  check "valid strings with a non-digit added at the end become invalid":
    is-valid("059a") is false
  end
end

fun invalid-number-after-punctuation-added():
  check "valid strings with punctuation included become invalid":
    is-valid("055-444-285") is false
  end
end

fun invalid-number-after-symbols-added():
  check "valid strings with symbols included become invalid":
    is-valid("055# 444$ 285") is false
  end
end

fun invalid-space-and-zero():
  check "single zero with space is invalid":
    is-valid(" 0") is false
  end
end

fun valid-multiple-zeroes():
  check "more than a single zero is valid":
    is-valid("0000 0") is true
  end
end

fun valid-input-digit-nine():
  check "input digit 9 is correctly converted to output digit 9":
    is-valid("091") is true
  end
end

fun valid-very-long-input():
  check "very long input is valid":
    is-valid("9999999999 9999999999 9999999999 9999999999") is true
  end
end

fun valid-with-odd-number-of-digits-and-non-zero-first-digit():
  check "valid luhn with an odd number of digits and non zero first digit":
    is-valid("109") is true
  end
end

fun invalid-number-with-ascii-non-double():
  check "using ascii value for non-doubled non-digit isn't allowed":
    is-valid("055b 444 285") is false
  end
end

fun invalid-number-with-ascii-doubled():
  check "using ascii value for doubled non-digit isn't allowed":
    is-valid(":9") is false
  end
end

fun invalid-number-with-non-space-letter():
  check "non-numeric, non-space char in the middle with a sum that's divisible by 10 isn't allowed":
    is-valid("59%59") is false
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, false) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(invalid-single-digit, true),
  test(invalid-single-zero, true),
  test(valid-even-if-reversed, true),
  test(valid-but-not-reversed, true),
  test(valid-canadian, true),
  test(invalid-canadian, true),
  test(invalid-credit-card, true),
  test(invalid-long-number-even-remainder, true),
  test(invalid-long-number-remainder-div-by-five, true),
  test(valid-number-with-even-number-of-digits, true),
  test(valid-number-with-odd-nummber-of-spaces, true),
  test(invalid-number-after-non-digit-added, true),
  test(invalid-number-after-punctuation-added, true),
  test(invalid-number-after-symbols-added, true),
  test(invalid-space-and-zero, true),
  test(valid-multiple-zeroes, true),
  test(valid-input-digit-nine, true),
  test(valid-very-long-input, true),
  test(valid-with-odd-number-of-digits-and-non-zero-first-digit, true),
  test(invalid-number-with-ascii-non-double, true),
  test(invalid-number-with-ascii-doubled, true),
  test(invalid-number-with-non-space-letter, true)
].each(lam(t): when t.active: t.run() end end)
