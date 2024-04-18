include std/unittest.e

include binary-search.ex

set_test_verbosity(TEST_SHOW_ALL)

test_equal("finds a value in a sequence with one element",
            1,
            my_find({6}, 6))
test_equal("finds a value in the middle of a sequence",
            4,
            my_find({1, 3, 4, 6, 8, 9, 11}, 6))
test_equal("finds a value at the beginning of a sequence",
            1,
            my_find({1, 3, 4, 6, 8, 9, 11}, 1))
test_equal("finds a value at the end of a sequence",
            7,
            my_find({1, 3, 4, 6, 8, 9, 11}, 11))
test_equal("finds a value in a sequence of odd length",
            10,
            my_find({1, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 634}, 144))
test_equal("finds a value in a sequence of even length",
            6,
            my_find({1, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377}, 21))
test_equal("identifies that a value is not included in the sequence",
            -1,
            my_find({1, 3, 4, 6, 8, 9, 11}, 7))
test_equal("a value smaller than the sequence's smallest value is not found",
            -1,
            my_find({1, 3, 4, 6, 8, 9, 11}, 0))
test_equal("a value smaller than the sequence's largest value is not found",
            -1,
            my_find({1, 3, 4, 6, 8, 9, 11}, 13))
test_equal("nothing is found in an empty array",
            -1,
            my_find({}, 1))
test_equal("nothing is found when the left and right bounds cross",
            -1,
            my_find({1, 2}, 0))

test_report()
