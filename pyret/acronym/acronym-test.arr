use context essentials2020

include file("acronym.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun basic():
  check "basic":
    abbreviate("Portable Network Graphics") is "PNG"
  end
end

fun lowercase-words():
  check "lowercase words":
    abbreviate("Ruby on Rails") is "ROR"
  end
end

fun punctuation():
  check "punctuation":
    abbreviate("First In, First Out") is "FIFO"
  end
end

fun all-caps-word():
  check "all caps word":
    abbreviate("GNU Image Manipulation Program") is "GIMP"
  end
end

fun punctuation-without-whitespace():
  check "punctuation without whitespace":
    abbreviate("Complementary metal-oxide semiconductor") is "CMOS"
  end
end

fun very-long-abbreviation():
  check "very long abbreviation":
    abbreviate("Rolling On The Floor Laughing So Hard That My Dogs Came Over And Licked Me") is "ROTFLSHTMDCOALM"
  end
end

fun consecutive-delimiters():
  check "consecutive delimiters":
    abbreviate("Something - I made up from thin air") is "SIMUFTA"
  end
end

fun apostrophes():
  check "apostrophes":
    abbreviate("Halley's Comet") is "HC"
  end
end

fun underscore-emphasis():
  check "underscore emphasis":
    abbreviate("The Road _Not_ Taken") is "TRNT"
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, false) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(basic, true),
  test(lowercase-words, false),
  test(punctuation, false),
  test(all-caps-word, false),
  test(punctuation-without-whitespace, false),
  test(very-long-abbreviation, false),
  test(consecutive-delimiters, false),
  test(apostrophes, false),
  test(underscore-emphasis, false)
].each(lam(t): when t.active: t.run() end end)