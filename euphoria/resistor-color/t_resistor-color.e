include std/unittest.e

include resistor-color.ex

set_test_verbosity(TEST_SHOW_ALL)

sequence EXPECTED_COLORS = {
  "black",
  "brown",
  "red",
  "orange",
  "yellow",
  "green",
  "blue",
  "violet",
  "grey",
  "white"
}

test_equal("Black", 0, colorCode("black"))
test_equal("White", 9, colorCode("white"))
test_equal("Orange", 3, colorCode("orange"))
test_equal("Colors", EXPECTED_COLORS, colors())

test_report()
