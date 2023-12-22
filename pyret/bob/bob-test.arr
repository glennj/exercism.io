use context essentials2020

include file("bob.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun stating-something():
  check "stating something":
    input = "Tom-ay-to, tom-aaaah-to."
    expected = "Whatever."

    response(input) is expected
  end
end

fun shouting():
  check "shouting":
    input = "WATCH OUT!"
    expected = "Whoa, chill out!"
    
    response(input) is expected
  end
end

fun shouting-gibberish():
  check "shouting gibberish":
    input = "FCECDFCAAB"
    expected = "Whoa, chill out!"
    
    response(input) is expected
  end
end

fun asking-a-question():
  check "asking a question":
    input = "Does this cryogenic chamber make me look fat?"
    expected = "Sure."
    
    response(input) is expected
  end
end

fun asking-numeric-question():
  check "asking a numeric question":
    input = "You are, what, like 15?"
    expected = "Sure."
    
    response(input) is expected
  end
end

fun asking-gibberish():
  check "asking gibberish":
    input = "fffbbcbeab?"
    expected = "Sure."
    
    response(input) is expected
  end
end

fun talking-forcefully():
  check "talking forcefully":
    input = "Hi there!"
    expected = "Whatever."
    
    response(input) is expected
  end
end

fun using-acronyms():
  check "using acronyms in regular speech":
    input = "It's OK if you don't want to go work for NASA."
    expected = "Whatever."
    
    response(input) is expected
  end
end

fun forceful-question():
  check "forceful question":
    input = "WHAT'S GOING ON?"
    expected = "Calm down, I know what I'm doing!"
    
    response(input) is expected
  end
end

fun shouting-numbers():
  check "shouting numbers":
    input = "1, 2, 3 GO!"
    expected = "Whoa, chill out!"

    response(input) is expected
  end
end

fun no-letters():
  check "no letters":
    input = "1, 2, 3"
    expected = "Whatever."

    response(input) is expected
  end
end

fun question-with-no-letters():
  check "question with no letters":
    input = "4?"
    expected = "Sure."

    response(input) is expected
  end
end

fun shouting-with-special-characters():
  check "shouting with special characters":
    input = "ZOMG THE %^*@#$(*^ ZOMBIES ARE COMING!!11!!1!"
    expected = "Whoa, chill out!"

    response(input) is expected
  end
end

fun shouting-with-no-exclamation-mark():
  check "shouting with no exclamation mark":
    input = "I HATE THE DENTIST"
    expected = "Whoa, chill out!"

    response(input) is expected
  end
end

fun statement-with-question-mark():
  check "statement containing question mark":
    input = "Ending with ? means a question."
    expected = "Whatever."

    response(input) is expected
  end
end

fun non-letters-with-question():
  check "non-letters with question":
    input = ":) ?"
    expected = "Sure."

    response(input) is expected
  end
end

fun prattling-on():
  check "prattling on":
    input = "Wait! Hang on. Are you going to be OK?"
    expected = "Sure."

    response(input) is expected
  end
end

fun silence():
  check "silence":
    input = ""
    expected = "Fine. Be that way!"

    response(input) is expected
  end
end

fun prolonged-silence():
  check "prolonged silence":
    input = "          "
    expected = "Fine. Be that way!"

    response(input) is expected
  end
end

fun alternate-silence():
  check "alternate silence":
    input = "\t\t\t\t\t\t\t\t\t\t"
    expected = "Fine. Be that way!"

    response(input) is expected
  end
end

fun multiple-line-question():
  check "multiple line question":
    input = "\nDoes this cryogenic chamber make me look fat?\nNo."
    expected = "Whatever."

    response(input) is expected
  end
end

fun starting-with-whitespace():
  check "starting with whitespace":
    input = "         hmmmmmmm..."
    expected = "Whatever."

    response(input) is expected
  end
end

fun ending-with-whitespace():
  check "ending with whitespace":
    input = "Okay if like my  spacebar  quite a bit?   "
    expected = "Sure."

    response(input) is expected
  end
end

fun other-whitespace():
  check "other whitespace":
    input = "\n\r \t"
    expected = "Fine. Be that way!"

    response(input) is expected
  end
end

fun non-question-ending-with-whitespace():
  check "non-question ending with whitespace":
    input = "This is a statement ending with whitespace      "
    expected = "Whatever."

    response(input) is expected
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, false) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(stating-something, true),
  test(shouting, true),
  test(shouting-gibberish, true),
  test(asking-a-question, true),
  test(asking-numeric-question, true),
  test(asking-gibberish, true),
  test(talking-forcefully, true),
  test(using-acronyms, true),
  test(forceful-question, true),
  test(shouting-numbers, true),
  test(no-letters, true),
  test(question-with-no-letters, true),
  test(shouting-with-special-characters, true),
  test(shouting-with-no-exclamation-mark, true),
  test(statement-with-question-mark, true),
  test(non-letters-with-question, true),
  test(prattling-on, true),
  test(silence, true),
  test(prolonged-silence, true),
  test(alternate-silence, true),
  test(multiple-line-question, true),
  test(starting-with-whitespace, true),
  test(ending-with-whitespace, true),
  test(other-whitespace, true),
  test(non-question-ending-with-whitespace, true)
].each(lam(t): when t.active: t.run() end end)