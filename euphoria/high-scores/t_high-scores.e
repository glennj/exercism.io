include std/unittest.e

include high-scores.ex

set_test_verbosity(TEST_SHOW_ALL)

test_equal("Latest score",
           30,
           latest({100, 0, 90, 30}))
test_equal("Personal best",
           100,
           personal_best({100, 0, 90, 30}))
test_equal("Personal top three from a list of scores",
           {100, 90, 70},
           personal_top_three({0, 90, 30, 100, 20, 10, 0, 30, 40, 40, 70, 70}))
test_equal("Personal top three highest to lowest",
           {30, 20, 10},
           personal_top_three({20, 10, 30}))
test_equal("Personal top three when there is a tie",
           {40, 40, 30},
           personal_top_three({40, 20, 40, 30}))
test_equal("Personal top three when there are less than three",
           {70, 30},
           personal_top_three({30, 70}))
test_equal("Personal top three when there is only one",
           {40},
           personal_top_three({40}))

test_report()
