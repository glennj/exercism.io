use context essentials2020

include file("proverb.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun zero-pieces():
  check "zero pieces":
    recite([list: ]) is [list: ]
  end
end

fun one-piece():
  check "one piece":
    input = [list: "nail"]
    expected = [list: "And all for the want of a nail."]

    recite(input) is expected
  end
end

fun two-pieces():
  check "two pieces":
    input = [list: "nail", "shoe"]
    expected = [list:   "For want of a nail the shoe was lost.", 
                        "And all for the want of a nail."]

    recite(input) is expected
  end
end

fun three-pieces():
  check "three pieces":
    input = [list: "nail", "shoe", "horse"]
    expected = [list:   "For want of a nail the shoe was lost.",
                        "For want of a shoe the horse was lost.",
                        "And all for the want of a nail."]

    recite(input) is expected
  end
end

fun full-proverb():
  check "full proverb":
    input = [list: "nail", "shoe", "horse", "rider", "message", "battle", "kingdom"]
    expected = [list:   "For want of a nail the shoe was lost.",
                        "For want of a shoe the horse was lost.",
                        "For want of a horse the rider was lost.",
                        "For want of a rider the message was lost.",
                        "For want of a message the battle was lost.",
                        "For want of a battle the kingdom was lost.",
                        "And all for the want of a nail."]

    recite(input) is expected
  end
end

fun four-pieces-modernized():
  check "four pieces modernized":
    input = [list: "pin", "gun", "soldier", "battle"]
    expected = [list:   "For want of a pin the gun was lost.",
                        "For want of a gun the soldier was lost.",
                        "For want of a soldier the battle was lost.",
                        "And all for the want of a pin."]

    recite(input) is expected
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, true) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(zero-pieces, true),
  test(one-piece, true),
  test(two-pieces, true),
  test(three-pieces, true),
  test(full-proverb, true),
  test(four-pieces-modernized, true)
].each(lam(t): when t.active: t.run() end end)