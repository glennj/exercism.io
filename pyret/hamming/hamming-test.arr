use context essentials2020

include file("hamming.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun empty-strands():
  check "empty strands":
    distance("", "") is 0
  end
end

fun single-letter-identical-strands():
  check "single letter identical strands":
    distance("A", "A") is 0
  end
end

fun single-letter-different-strands():
  check "single letter different strands":
    distance("G", "T") is 1
  end
end

fun long-identical-strands():
  check "long identical strands":
    distance("GGACTGAAATCTG", "GGACTGAAATCTG") is 0
  end
end

fun long-different-strands():
  check "long different strands":
    distance("GGACGGATTCTG", "AGGACGGATTCT") is 9
  end
end

fun disallow-first-strand-longer():
  check "disallow first strand longer":
    distance("AATG", "AAA") raises "Strands must be of equal length."
  end
end

fun disallow-second-strand-longer():
  check "disallow second strand longer":
    distance("ATA", "AGTG") raises "Strands must be of equal length."
  end
end

fun disallow-empty-first-strand():
  check "disallow empty first strand":
    distance("", "G") raises "Strands must be of equal length."
  end
end

fun disallow-empty-second-strand():
  check "disallow empty second strand":
    distance("G", "") raises "Strands must be of equal length."
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, false) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(empty-strands, true),
  test(single-letter-identical-strands, true),
  test(single-letter-different-strands, true),
  test(long-identical-strands, true),
  test(long-different-strands, true),
  test(disallow-first-strand-longer, true),
  test(disallow-second-strand-longer, true),
  test(disallow-empty-first-strand, true),
  test(disallow-empty-second-strand, true)
].each(lam(t): when t.active: t.run() end end)
