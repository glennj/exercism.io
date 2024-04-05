include std/unittest.e 

include pangram.ex 

set_test_verbosity(TEST_SHOW_ALL)

test_equal("empty sentence",is_pangram(""),0)
test_equal("perfect lower case",is_pangram("abcdefghijklmnopqrstuvwxyz"),1)
test_equal("only lower case",is_pangram("the quick brown fox jumps over the lazy dog"),1)
test_equal("missing the letter 'x'",is_pangram("a quick movement of the enemy will jeopardize five gunboats"),0)
test_equal("missing the letter 'h'",is_pangram("five boxing wizards jump quickly at it"),0)
test_equal("with underscores",is_pangram("the_quick_brown_fox_jumps_over_the_lazy_dog"),1)
test_equal("with numbers",is_pangram("the 1 quick brown fox jumps over the 2 lazy dogs"),1)
test_equal("missing letters replaced by numbers",is_pangram("7h3 qu1ck brown fox jumps ov3r 7h3 lazy dog"),0)
test_equal("mixed case and punctuation",is_pangram("Five quacking Zephyrs jolt my wax bed."),1)
test_equal("case insensitive",is_pangram("the quick brown fox jumps over with lazy FX"),0)
test_equal("a-m and A-M are 26 different characters but not a pangram",is_pangram("abcdefghijklm ABCDEFGHIJKLM"),0)

test_report() 
