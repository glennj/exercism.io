use context essentials2020

include file("high-scores.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun list-of-scores():
  check "List of scores":
    scores = [list: 30, 50, 20, 70]
    expected = [list: 30, 50, 20, 70]

    high-scores(scores).scores is expected
  end
end

fun latest-score():
  check "Latest score":
    scores = [list: 100, 0, 90, 30]
    expected = 30

    high-scores(scores).latest() is expected
  end
end

fun personal-best():
  check "Personal best":
    scores = [list: 40, 100, 70]
    expected = 100

    high-scores(scores).personal-best() is expected
  end
end

fun top-three():
  check "Top 3 scores -> Personal top three from a list of scores":
    scores = [list: 10, 30, 90, 30, 100, 20, 10, 0, 30, 40, 40, 70, 70]
    expected = [list: 100, 90, 70]

    high-scores(scores).personal-top-three() is expected
  end
end

fun top-three-is-sorted():
  check "Top 3 scores -> Personal top highest to lowest":
    scores = [list: 20, 10, 30]
    expected = [list: 30, 20, 10]

    high-scores(scores).personal-top-three() is expected
  end
end

fun top-three-with-tie():
  check "Top 3 scores -> Personal top when there is a tie":
    scores = [list: 40, 20, 40, 30]
    expected = [list: 40, 40, 30]
    
    high-scores(scores).personal-top-three() is expected
  end
end

fun top-three-when-less-than-three():
  check "Top 3 scores -> Personal top when there are less than 3":
    scores = [list: 30, 70]
    expected = [list: 70, 30]
    
    high-scores(scores).personal-top-three() is expected
  end
end

fun top-three-when-only-one():
  check "Top 3 scores -> Personal top when there is only one":
    scores = [list: 40]
    expected = [list: 40]
    
    high-scores(scores).personal-top-three() is expected
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, true) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(list-of-scores, true),
  test(latest-score, true),
  test(personal-best, true),
  test(top-three, true),
  test(top-three-is-sorted, true),
  test(top-three-with-tie, true),
  test(top-three-when-less-than-three, true),
  test(top-three-when-only-one, true)
].each(lam(t): when t.active: t.run() end end)