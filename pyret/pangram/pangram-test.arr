use context essentials2020

include file("pangram.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun empty-sentence():
  check "empty sentence":
    input = ""
    is-pangram(input) is false
  end
end

fun perfect-lower-case():
  check "perfect lower case":
    input = "abcdefghijklmnopqrstuvwxyz"
    is-pangram(input) is true
  end
end

fun only-lower-case():
  check "only lower case":
    input = "the quick brown fox jumps over the lazy dog"
    is-pangram(input) is true
  end
end

fun missing-letter-x():
  check "missing the letter 'x'":
    input = "a quick movement of the enemy will jeopardize five gunboats"
    is-pangram(input) is false
  end
end

fun missing-letter-h():
  check "missing the letter 'h'":
    input = "five boxing wizards jump quickly at it"
    is-pangram(input) is false
  end
end

fun with-underscores():
  check "with underscores":
    input = "the_quick_brown_fox_jumps_over_the_lazy_dog"
    is-pangram(input) is true
  end
end

fun with-numbers():
  check "with numbers":
    input = "the 1 quick brown fox jumps over the 2 lazy dogs"
    is-pangram(input) is true
  end
end

fun missing-letters-replaced-by-numbers():
  check "missing letters replaced by numbers":
    input = "7h3 qu1ck brown fox jumps ov3r 7h3 lazy dog"
    is-pangram(input) is false
  end
end

fun mixed-case-and-punctuation():
  check "mixed case and punctuation":
    input = "\"Five quacking Zephyrs jolt my wax bed.\""
    is-pangram(input) is true
  end
end

fun length-of-alphabet-but-not-pangram():
  check "a-m and A-M are 26 different characters but not a pangram":
    input = "abcdefghijklm ABCDEFGHIJKLM"
    is-pangram(input) is false
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, false) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(empty-sentence, true),
  test(perfect-lower-case, false),
  test(only-lower-case, false),
  test(missing-letter-x, false),
  test(missing-letter-h, false),
  test(with-underscores, false),
  test(with-numbers, false),
  test(missing-letters-replaced-by-numbers, false),
  test(mixed-case-and-punctuation, false),
  test(length-of-alphabet-but-not-pangram, false)
].each(lam(t): when t.active: t.run() end end)