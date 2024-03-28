use context essentials2020

include file("robot-simulator.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun create-robot-at-origin-facing-north():
  check "Create robot at origin facing north":
    r = robot(0, 0, 'north')
    expected = robot(0, 0, 'north')
    r is expected
  end
end

fun create-robot-at-negative-position-facing-south():
  check "Create robot at negative position facing south":
    r = robot(-1, -1, 'south')
    expected = robot(-1, -1, 'south')
    r is expected
  end
end

fun rotating-clockwise-changes-north-to-east():
  check "Rotating clockwise changes north to east":
    r = robot(0, 0, 'north')
    expected = robot(0, 0, 'east')
    r.move('R') is expected
  end
end

fun rotating-clockwise-changes-east-to-south():
  check "Rotating clockwise changes east to south":
    r = robot(0, 0, 'east')
    expected = robot(0, 0, 'south')
    r.move('R') is expected
  end
end

fun rotating-clockwise-changes-south-to-west():
  check "Rotating clockwise changes south to west":
    r = robot(0, 0, 'south')
    expected = robot(0, 0, 'west')
    r.move('R') is expected
  end
end

fun rotating-clockwise-changes-west-to-north():
  check "Rotating clockwise changes west to north":
    r = robot(0, 0, 'west')
    expected = robot(0, 0, 'north')
    r.move('R') is expected
  end
end

fun rotating-counter-clockwise-changes-north-to-west():
  check "Rotating counter-clockwise changes north to west":
    r = robot(0, 0, 'north')
    expected = robot(0, 0, 'west')
    r.move('L') is expected
  end
end

fun rotating-counter-clockwise-changes-west-to-south():
  check "Rotating counter-clockwise changes west to south":
    r = robot(0, 0, 'west')
    expected = robot(0, 0, 'south')
    r.move('L') is expected
  end
end

fun rotating-counter-clockwise-changes-south-to-east():
  check "Rotating counter-clockwise changes south to east":
    r = robot(0, 0, 'south')
    expected = robot(0, 0, 'east')
    r.move('L') is expected
  end
end

fun rotating-counter-clockwise-changes-east-to-north():
  check "Rotating counter-clockwise changes east to north":
    r = robot(0, 0, 'east')
    expected = robot(0, 0, 'north')
    r.move('L') is expected
  end
end

fun moving-forward-one-facing-north-increments-Y():
  check "Moving forward one facing north increments Y":
    r = robot(0, 0, 'north')
    expected = robot(0, 1, 'north')
    r.move('A') is expected
  end
end

fun moving-forward-one-facing-south-decrements-Y():
  check "Moving forward one facing south decrements Y":
    r = robot(0, 0, 'south')
    expected = robot(0, -1, 'south')
    r.move('A') is expected
  end
end

fun moving-forward-one-facing-east-increments-X():
  check "Moving forward one facing east increments X":
    r = robot(0, 0, 'east')
    expected = robot(1, 0, 'east')
    r.move('A') is expected
  end
end

fun moving-forward-one-facing-west-decrements-X():
  check "Moving forward one facing west decrements X":
    r = robot(0, 0, 'west')
    expected = robot(-1, 0, 'west')
    r.move('A') is expected
  end
end

fun follow-series-of-directions-moving-east-and-north-from-readme():
  check "Follow series of directions moving east and north from README":
    r = robot(7, 3, 'north')
    expected = robot(9, 4, 'west')
    r.move('RAALAL') is expected
  end
end

fun follow-series-of-directions-moving-west-and-north():
  check "Follow series of directions moving west and north":
    r = robot(0, 0, 'north')
    expected = robot(-4, 1, 'west')
    r.move('LAAARALA') is expected
  end
end

fun follow-series-of-directions-moving-west-and-south():
  check "Follow series of directions moving west and south":
    r = robot(2, -7, 'east')
    expected = robot(-3, -8, 'south')
    r.move('RRAAAAALA') is expected
  end
end

fun follow-series-of-directions-moving-east-and-north():
  check "Follow series of directions moving east and north":
    r = robot(8, 4, 'south')
    expected = robot(11, 5, 'north')
    r.move('LAAARRRALLLL') is expected
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, true) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(create-robot-at-origin-facing-north, true),
  test(create-robot-at-negative-position-facing-south, true),
  test(rotating-clockwise-changes-north-to-east, true),
  test(rotating-clockwise-changes-east-to-south, true),
  test(rotating-clockwise-changes-south-to-west, true),
  test(rotating-clockwise-changes-west-to-north, true),
  test(rotating-counter-clockwise-changes-north-to-west, true),
  test(rotating-counter-clockwise-changes-west-to-south, true),
  test(rotating-counter-clockwise-changes-south-to-east, true),
  test(rotating-counter-clockwise-changes-east-to-north, true),
  test(moving-forward-one-facing-north-increments-Y, true),
  test(moving-forward-one-facing-south-decrements-Y, true),
  test(moving-forward-one-facing-east-increments-X, true),
  test(moving-forward-one-facing-west-decrements-X, true),
  test(follow-series-of-directions-moving-east-and-north-from-readme, true),
  test(follow-series-of-directions-moving-west-and-north, true),
  test(follow-series-of-directions-moving-west-and-south, true),
  test(follow-series-of-directions-moving-east-and-north, true)
].each(lam(t): when t.active: t.run() end end)
