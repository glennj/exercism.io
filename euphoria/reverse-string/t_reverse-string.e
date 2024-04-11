include std/unittest.e

include reverse-string.ex

set_test_verbosity(TEST_SHOW_ALL)

test_equal("an empty string", "", reverse(""))
test_equal("a word", "tobor", reverse("robot"))
test_equal("a capitalized word", "nemaR", reverse("Ramen"))
test_equal("a sentence with punctuation", "!yrgnuh m'I", reverse("I'm hungry!"))
test_equal("a palindrome", "racecar", reverse("racecar"))
test_equal("an even-sized word", "reward", reverse("drawer"))

test_report()
