use context essentials2020

include file("scrabble-score.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun lowercase-letter():
  check "lowercase letter":
    score("a") is 1
  end
end

fun uppercase-letter():
  check "uppercase letter":
    score("A") is 1
  end
end

fun valuable-letter():
  check "valuable letter":
    score("f") is 4
  end
end

fun short-word():
  check "short word":
    score("at") is 2
  end
end

fun short-valuable-word():
  check "short, valuable word":
    score("zoo") is 12
  end
end

fun medium-word():
  check "medium word":
    score("street") is 6
  end
end

fun medium-valuable-word():
  check "medium, valuable word":
    score("quirky") is 22
  end
end

fun long-mixed-case-word():
  check "long, mixed-case word":
    score("OxyphenButazone") is 41
  end
end

fun english-like-word():
  check "english-like word":
    score("pinata") is 8
  end
end

fun empty-input():
  check "empty input":
    score("") is 0
  end
end

fun entire-alphabet():
  check "entire alphabet available":
    score("abcdefghijklmnopqrstuvwxyz") is 87
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, false) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(lowercase-letter, true),
  test(uppercase-letter, true),
  test(valuable-letter, true),
  test(short-word, true),
  test(short-valuable-word, true),
  test(medium-word, true),
  test(medium-valuable-word, true),
  test(long-mixed-case-word, true),
  test(english-like-word, true),
  test(empty-input, true),
  test(entire-alphabet, true)
].each(lam(t): when t.active: t.run() end end)
