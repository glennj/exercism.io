use context essentials2020

include file("word-count.arr")

include string-dict

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun count-one-word():
  check "count one word":
    input = "word"
    expected = [string-dict: "word", 1]
    
    word-count(input) is expected
  end
end

fun count-one-of-each-word():
  check "count one of each word":
    input = "one of each"
    expected = [string-dict: "one", 1, "of", 1, "each", 1]
    
    word-count(input) is expected
  end
end

fun multiple-occurences-of-word():
  check "multiple occurrences of a word":
    input = "one fish two fish red fish blue fish"
    expected = [string-dict: "one", 1, "fish", 4, "two", 1, "red", 1, "blue", 1]
    
    word-count(input) is expected
  end
end

fun handle-cramped-lists():
  check "handles cramped lists":
    input = "one,two,three"
    expected = [string-dict: "one", 1, "two", 1, "three", 1]
    
    word-count(input) is expected
  end
end

fun handle-expanded-lists():
  check "handles expanded lists":
    input = "one,\ntwo,\nthree"
    expected = [string-dict: "one", 1, "two", 1, "three", 1]
    
    word-count(input) is expected
  end
end

fun ignore-punctuation():
  check "ignore punctuation":
    input = "car: carpet as java: javascript!!&@$%^&"
    expected = [string-dict:
      "car", 1,
      "carpet", 1,
      "as", 1,
      "java", 1,
      "javascript", 1]
    
    word-count(input) is expected
  end
end

fun include-numbers():
  check "include numbers":
    input = "testing, 1, 2 testing"
    expected = [string-dict: "testing", 2, "1", 1, "2", 1]
    
    word-count(input) is expected
  end
end

fun normalize-case():
  check "normalize case":
    input = "go Go GO Stop stop"
    expected = [string-dict: "go", 3, "stop", 2]
    
    word-count(input) is expected
  end
end

fun with-apostrophes():
  check "with apostrophes":
    input = "'First: don't laugh. Then: don't cry. You're getting it.'"
    expected = [string-dict: 
      "first", 1,
      "don't", 2,
      "laugh", 1,
      "then", 1,
      "cry", 1,
      "you're", 1,
      "getting", 1,
      "it", 1]
    
    word-count(input) is expected
  end
end

fun with-quotations():
  check "with quotations":
    input = "Joe can't tell between 'large' and large."
    expected = [string-dict:
      "joe", 1,
      "can't", 1,
      "tell", 1,
      "between", 1,
      "large", 2,
      "and", 1]
    
    word-count(input) is expected
  end
end

fun substrings-from-the-beginning():
  check "substrings from the beginning":
    input = "Joe can't tell between app, apple and a."
    expected = [string-dict:
      "joe", 1,
      "can't", 1,
      "tell", 1,
      "between", 1,
      "app", 1,
      "apple", 1,
      "and", 1,
      "a", 1]
    
    word-count(input) is expected
  end
end

fun multiple-spaces-not-a-word():
  check "multiple spaces not detected as a word":
    input = " multiple   whitespaces"
    expected = [string-dict: "multiple", 1, "whitespaces", 1]
    
    word-count(input) is expected
  end
end

fun alternating-word-separators-not-a-word():
  check "alternating word separators not detected as a word":
    input = ",\n,one,\n ,two \n 'three'"
    expected = [string-dict: "one", 1, "two", 1, "three", 1]
    
    word-count(input) is expected
  end
end

fun quotation-for-word-with-apostrophe():
  check "quotation for word with apostrophe":
    input = "can, can't, 'can't'"
    expected = [string-dict: "can", 1, "can't", 2]

    word-count(input) is expected
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, true) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(count-one-word, true),
  test(count-one-of-each-word, true),
  test(multiple-occurences-of-word, true),
  test(handle-cramped-lists, true),
  test(handle-expanded-lists, true),
  test(ignore-punctuation, true),
  test(include-numbers, true),
  test(normalize-case, true),
  test(with-apostrophes, true),
  test(with-quotations, true),
  test(substrings-from-the-beginning, true),
  test(multiple-spaces-not-a-word, true),
  test(alternating-word-separators-not-a-word, true),
  test(quotation-for-word-with-apostrophe, true)
].each(lam(t): when t.active: t.run() end end)
