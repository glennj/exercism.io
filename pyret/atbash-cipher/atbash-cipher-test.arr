use context essentials2020

include file("atbash-cipher.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun encode-yes():
  check "encode -> encode yes":
    input = "yes"
    expected = "bvh"

    encode(input) is expected
  end
end

fun encode-no():
  check "encode -> encode no":
    input = "no"
    expected = "ml"
    
    encode(input) is expected
  end
end

fun encode-OMG():
  check "encode -> encode OMG":
    input = "OMG"
    expected = "lnt"
    
    encode(input) is expected
  end
end

fun encode-spaces():
  check "encode -> encode spaces":
    input = "O M G"
    expected = "lnt"
    
    encode(input) is expected
  end
end

fun encode-mindblowingly():
  check "encode -> encode mindblowingly":
    input = "mindblowingly"
    expected = "nrmwy oldrm tob"
    
    encode(input) is expected
  end
end

fun encode-numbers():
  check "encode -> encode numbers":
    input = "Testing,1 2 3, testing."
    expected = "gvhgr mt123 gvhgr mt"
    
    encode(input) is expected
  end
end

fun encode-deep-thought():
  check "encode -> encode deep thought":
    input = "Truth is fiction."
    expected = "gifgs rhurx grlm"
    
    encode(input) is expected
  end
end

fun encode-all-letters():
  check "encode -> encode all the letters":
    input = "The quick brown fox jumps over the lazy dog."
    expected = "gsvjf rxpyi ldmul cqfnk hlevi gsvoz abwlt"
    
    encode(input) is expected
  end
end

fun decode-exercism():
  check "decode -> decode exercism":
    input = "vcvix rhn"
    expected = "exercism"
    
    decode(input) is expected
  end
end

fun decode-sentence():
  check "decode -> decode a sentence":
    input = "zmlyh gzxov rhlug vmzhg vkkrm thglm v"
    expected = "anobstacleisoftenasteppingstone"
    
    decode(input) is expected
  end
end

fun decode-numbers():
  check "decode -> decode numbers":
    input = "gvhgr mt123 gvhgr mt"
    expected = "testing123testing"
    
    decode(input) is expected
  end
end

fun decode-all-letters():
  check "decode -> decode all the letters":
    input = "gsvjf rxpyi ldmul cqfnk hlevi gsvoz abwlt"
    expected = "thequickbrownfoxjumpsoverthelazydog"
    
    decode(input) is expected
  end
end

fun decode-with-too-many-spaces():
  check "decode -> decode with too many spaces":
    input = "vc vix    r hn"
    expected = "exercism"
    
    decode(input) is expected
  end
end

fun decode-with-no-spaces():
  check "decode -> decode with no spaces":
    input = "zmlyhgzxovrhlugvmzhgvkkrmthglmv"
    expected = "anobstacleisoftenasteppingstone"
    
    decode(input) is expected
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, true) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(encode-yes, true),
  test(encode-no, true),
  test(encode-OMG, true),
  test(encode-spaces, true),
  test(encode-mindblowingly, true),
  test(encode-numbers, true),
  test(encode-deep-thought, true),
  test(encode-all-letters, true),
  test(decode-exercism, true),
  test(decode-sentence, true),
  test(decode-numbers, true),
  test(decode-all-letters, true),
  test(decode-with-too-many-spaces, true),
  test(decode-with-no-spaces, true)
].each(lam(t): when t.active: t.run() end end)