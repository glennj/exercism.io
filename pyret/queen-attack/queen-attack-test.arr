use context essentials2020

include file("queen-attack.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun valid-position():
  check "Test creation of Queens with valid and invalid positions -> queen with a valid position":
    queen(2, 2) does-not-raise
  end
end

fun invalid-negative-row():
  check "Test creation of Queens with valid and invalid positions -> queen must have positive row":
    queen(-2, 2) raises "" # matches any exception message
  end
end

fun invalid-row-not-on-board():
  check "Test creation of Queens with valid and invalid positions -> queen must have row on board":
    queen(8, 4) raises "" # matches any exception message
  end
end

fun invalid-negative-column():
  check "Test creation of Queens with valid and invalid positions -> queen must have positive column":
    queen(2, -2) raises "" # matches any exception message
  end
end

fun invalid-column-not-on-board():
  check "Test creation of Queens with valid and invalid positions -> queen must have column on board":
    queen(4, 8) raises "" # matches any exception message
  end
end

fun can-not-attack():
  check "Test the ability of one queen to attack another -> cannot attack":
    queen(2, 4).can-attack(queen(6, 6)) is false
  end
end

fun can-attack-on-same-row():
  check "Test the ability of one queen to attack another -> can attack on same row":
    queen(2, 4).can-attack(queen(2, 6)) is true
  end
end

fun can-attack-on-same-column():
  check "Test the ability of one queen to attack another -> can attack on same column":
    queen(4, 5).can-attack(queen(2, 5)) is true
  end
end

fun can-attack-on-first-diagonal():
  check "Test the ability of one queen to attack another -> can attack on first diagonal":
    queen(2, 2).can-attack(queen(0, 4)) is true
  end
end

fun can-attack-on-second-diagonal():
  check "Test the ability of one queen to attack another -> can attack on second diagonal":
    queen(2, 2).can-attack(queen(3, 1)) is true
  end
end

fun can-attack-on-third-diagonal():
  check "Test the ability of one queen to attack another -> can attack on third diagonal":
    queen(2, 2).can-attack(queen(1, 1)) is true
  end
end

fun can-attack-on-fourth-diagonal():
  check "Test the ability of one queen to attack another -> can attack on fourth diagonal":
    queen(1, 7).can-attack(queen(0, 6)) is true
  end
end

fun can-not-attack-on-reflected-diagonal():
  check "Test the ability of one queen to attack another -> cannot attack if falling diagonals are only the same when reflected across the longest falling diagonal":
    queen(4, 1).can-attack(queen(2, 5)) is false
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, false) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(valid-position, true),
  test(invalid-negative-row, true),
  test(invalid-row-not-on-board, true),
  test(invalid-negative-column, true),
  test(invalid-column-not-on-board, true),
  test(can-not-attack, true),
  test(can-attack-on-same-row, true),
  test(can-attack-on-same-column, true),
  test(can-attack-on-first-diagonal, true),
  test(can-attack-on-second-diagonal, true),
  test(can-attack-on-third-diagonal, true),
  test(can-attack-on-fourth-diagonal, true),
  test(can-not-attack-on-reflected-diagonal, true)
].each(lam(t): when t.active: t.run() end end)