use context essentials2020

include file("isogram.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun empty-string():
  check "empty string":
    is-isogram("") is true
  end
end

fun only-lowercase():
  check "isogram with only lower case characters":
    is-isogram("isogram") is true
  end
end

fun duplicated-character():
  check "word with one duplicated character":
    is-isogram("eleven") is false
  end
end

fun duplicate-character-from-end-of-alphabet():
  check "word with one duplicated character from the end of the alphabet":
    is-isogram("zzyzx") is false
  end
end

fun longest-reported-isogram():
  check "longest reported english isogram":
    is-isogram("subdermatoglyphic") is true
  end
end

fun duplicate-character-mixed-case():
  check "word with duplicated character in mixed case":
    is-isogram("Alphabet") is false
  end
end

fun duplicated-character-mixed-case-lowercase-first():
  check "word with duplicated character in mixed case, lowercase first":
    is-isogram("alphAbet") is false
  end
end

fun isogram-with-hyphen():
  check "hypothetical isogrammic word with hyphen":
    is-isogram("thumbscrew-japingly") is true
  end
end

fun isogram-with-hyphen-and-duplicate-character():
  check "hypothetical word with duplicated character following hyphen":
    is-isogram("thumbscrew-jappingly") is false
  end
end

fun isogram-with-duplicated-hyphen():
  check "isogram with duplicated hyphen":
    is-isogram("six-year-old") is true
  end
end

fun made-up-name():
  check "made-up name that is an isogram":
    is-isogram("Emily Jung Schwartzkopf") is true
  end
end

fun duplicate-character-in-middle():
  check "duplicated character in the middle":
    is-isogram("accentor") is false
  end
end

fun duplicate-character-on-ends():
  check "same first and last characters":
    is-isogram("angola") is false
  end
end

fun duplicate-character-and-hyphens():
  check "word with duplicated character and with two hyphens":
    is-isogram("up-to-date") is false
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, false) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(empty-string, true),
  test(only-lowercase, true),
  test(duplicated-character, true),
  test(duplicate-character-from-end-of-alphabet, true),
  test(longest-reported-isogram, true),
  test(duplicate-character-mixed-case, true),
  test(duplicated-character-mixed-case-lowercase-first, true),
  test(isogram-with-hyphen, true),
  test(isogram-with-hyphen-and-duplicate-character, true),
  test(isogram-with-duplicated-hyphen, true),
  test(made-up-name, true),
  test(duplicate-character-in-middle, true),
  test(duplicate-character-on-ends, true),
  test(duplicate-character-and-hyphens, true)
].each(lam(t): when t.active: t.run() end end)
