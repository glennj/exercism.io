include std/unittest.e

include scrabble-score.ex

set_test_verbosity(TEST_SHOW_ALL)

test_equal("lowercase letter", 1, score("a"))
test_equal("uppercase letter", 1, score("A"))
test_equal("valuable letter", 4, score("f"))
test_equal("short word", 2, score("at"))
test_equal("short, valuable word", 12, score("zoo"))
test_equal("medium word", 6, score("street"))
test_equal("medium, valuable word", 22, score("quirky"))
test_equal("long, mixed-case word", 41, score("OxyphenButazone"))
test_equal("english-like word", 8, score("pinata"))
test_equal("empty input", 0, score(""))
test_equal("entire alphabet available", 87, score("abcdefghijklmnopqrstuvwxyz"))

test_report()
