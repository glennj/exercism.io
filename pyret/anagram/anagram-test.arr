use context essentials2020

include file("anagram.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun no-matches():
  check "no matches":
    candidates = [list: "hello", "world", "zombies", "pants"]
    expected = [list: ]

    find-anagrams("diaper", candidates) is expected
  end
end

fun detect-two-anagrams():
  check "detects two anagrams":
    candidates = [list: "lemons", "cherry", "melons"]
    expected = [list: "lemons", "melons"]

    find-anagrams("solemn", candidates) is expected
  end
end

fun no-detect-anagram-subsets():
  check "does not detect anagram subsets":
    candidates = [list: "dog", "goody"]
    expected = [list: ]

    find-anagrams("good", candidates) is expected
  end
end

fun detect-anagram():
  check "detects anagram":
    candidates = [list: "enlists", "google", "inlets", "banana"]
    expected = [list: "inlets"]

    find-anagrams("listen", candidates) is expected
  end
end

fun detect-three-anagrams():
  check "detects three anagrams":
    candidates = [list: "gallery", "ballerina", "regally", "clergy", "largely", "leading"]
    expected = [list: "gallery", "regally", "largely"]

    find-anagrams("allergy", candidates) is expected
  end
end

fun detect-multiple-anagrams-with-diff-case():
  check "detects multiple anagrams with different case":
    candidates = [list: "Eons", "ONES"]
    expected = [list: "Eons", "ONES"]

    find-anagrams("nose", candidates) is expected
  end
end

fun no-detect-identical-checksum():
  check "does not detect non-anagrams with identical checksum":
    candidates = [list: "last"]
    expected = [list: ]

    find-anagrams("mass", candidates) is expected
  end
end

fun detect-anagram-case-insensitively():
  check "detects anagrams case-insensitively":
    candidates = [list: "cashregister", "Carthorse", "radishes"]
    expected = [list: "Carthorse"]

    find-anagrams("Orchestra", candidates) is expected
  end
end

fun detect-anagram-case-insensitive-subject():
  check "detects anagrams using case-insensitive subject":
    candidates = [list: "cashregister", "carthorse", "radishes"]
    expected = [list: "carthorse"]

    find-anagrams("Orchestra", candidates) is expected
  end
end

fun detect-anagram-case-insensitive-candidates():
  check "detects anagrams using case-insensitive possible matches":
    candidates = [list: "cashregister", "Carthorse", "radishes"]
    expected = [list: "Carthorse"]

    find-anagrams("orchestra", candidates) is expected
  end
end

fun no-detect-anagram-for-repeating-word():
  check "does not detect an anagram if the original word is repeated":
    candidates = [list: "go Go GO"]
    expected = [list: ]

    find-anagrams("go", candidates) is expected
  end
end

fun anagrams-use-all-letters-once():
  check "anagrams must use all letters exactly once":
    candidates = [list: "patter"]
    expected = [list: ]

    find-anagrams("tapper", candidates) is expected
  end
end

fun words-are-not-anagrams-of-themselves():
  check "words are not anagrams of themselves":
    candidates = [list: "BANANA"]
    expected = [list: ]

    find-anagrams("BANANA", candidates) is expected
  end
end

fun words-are-not-anagrams-of-themselves-even-if-case-partially-different():
  check "words are not anagrams of themselves even if letter case is partially different":
    candidates = [list: "Banana"]
    expected = [list: ]

    find-anagrams("BANANA", candidates) is expected
  end
end

fun words-are-not-anagrams-of-themselves-even-if-case-completely-different():
  check "words are not anagrams of themselves even if letter case is completely different":
    candidates = [list: "banana"]
    expected = [list: ]

    find-anagrams("BANANA", candidates) is expected
  end
end

fun words-other-than-self-can-be-anagram():
  check "words other than themselves can be anagrams":
    candidates = [list: "LISTEN", "Silent"]
    expected = [list: "Silent"]

    find-anagrams("LISTEN", candidates) is expected
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, false) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(no-matches, true),
  test(detect-two-anagrams, true),
  test(no-detect-anagram-subsets, true),
  test(detect-anagram, true),
  test(detect-three-anagrams, true),
  test(detect-multiple-anagrams-with-diff-case, true),
  test(no-detect-identical-checksum, true),
  test(detect-anagram-case-insensitively, true),
  test(detect-anagram-case-insensitive-subject, true),
  test(detect-anagram-case-insensitive-candidates, true),
  test(no-detect-anagram-for-repeating-word, true),
  test(anagrams-use-all-letters-once, true),
  test(words-are-not-anagrams-of-themselves, true),
  test(words-are-not-anagrams-of-themselves-even-if-case-partially-different, true),
  test(words-are-not-anagrams-of-themselves-even-if-case-completely-different, true),
  test(words-other-than-self-can-be-anagram, true)
].each(lam(t): when t.active: t.run() end end)