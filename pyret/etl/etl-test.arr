use context essentials2020

include file("etl.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

include string-dict

fun single-letter():
  check "single letter":
    translate([string-dict: "1", [list: "A"]]) is [string-dict: "a", "1"]
  end
end

fun single-score-multiple-letters():
  check "single score with multiple letters":
    translate([string-dict: "1", [list: "A", "E", "I", "O", "U"]]) is [string-dict: "a", "1", "e", "1", "i", "1", "o", "1", "u", "1"]
  end
end

fun multiple-scores-multiple-letters():
  check "multiple scores with multiple letters":
    input = [string-dict: "1", [list: "A", "E", "I", "O", "U"], "2", [list: "D", "G"]]
    expected = [string-dict: "a", "1", "e", "1", "i", "1", "o", "1", "u", "1", "d", "2", "g", "2"]
    translate(input) is expected
  end
end

fun multiple-scores-different-numbers-of-letters():
  check "multiple scores with differing numbers of letters":
    input = [string-dict: "1", [list: "A", "E", "I", "O", "U"], 
                          "2", [list: "D", "G"], 
                          "3", [list: "B", "C", "M", "P"], 
                          "4", [list: "F", "H", "V", "W", "Y"], 
                          "5", [list: "K"], 
                          "8", [list: "J", "X"], 
                          "10", [list: "Q", "Z"]]
    expected = [string-dict: "a", "1", "e", "1", "i", "1", "o", "1", "u", "1", 
                                    "d", "2", "g", "2", 
                                    "b", "3", "c", "3", "m", "3", "p", "3",
                                    "f", "4", "h", "4", "v", "4", "w", "4", "y", "4",
                                    "k", "5", 
                                    "j", "8", "x", "8", 
                                    "q", "10", "z", "10"]
    translate(input) is expected
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, false) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(single-letter, true),
  test(single-score-multiple-letters, true),
  test(multiple-scores-multiple-letters, true),
  test(multiple-scores-different-numbers-of-letters, true)
].each(lam(t): when t.active: t.run() end end)
