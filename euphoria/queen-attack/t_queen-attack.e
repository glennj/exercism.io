include std/unittest.e

include queen-attack.ex

set_test_verbosity(TEST_SHOW_ALL)

test_true("Test creation of queens - valid position",
           queen({2, 2}))
test_false("Test creation of queens - must have positive row",
           queen({-2, 2}))
test_false("Test creation of queens - must have row on board",
           queen({8, 4}))
test_false("Test creation of queens - must have positive column",
           queen({2, -2}))
test_false("Test creation of queens - must have column on board",
           queen({4, 8}))
test_false("Test the ability of one queen to attack another - cannot attack",
           can_attack({2, 4}, {6, 6}))
test_true("Test the ability of one queen to attack another - can attack on same row",
          can_attack({2, 4}, {2, 6}))
test_true("Test the ability of one queen to attack another - can attack on same column",
          can_attack({4, 5}, {2, 5}))
test_true("Test the ability of one queen to attack another - can attack on first diagonal",
          can_attack({2, 2}, {0, 4}))
test_true("Test the ability of one queen to attack another - can attack on second diagonal",
          can_attack({2, 2}, {3, 1}))
test_true("Test the ability of one queen to attack another - can attack on third diagonal",
          can_attack({2, 2}, {1, 1}))
test_true("Test the ability of one queen to attack another - can attack on fourth diagonal",
          can_attack({1, 7}, {0, 6}))
test_false("Test the ability of one queen to attack another - cannot attack if falling diagonals are only the same when reflected across the longest falling diagonal",
          can_attack({4, 1}, {2, 5}))

test_report()
