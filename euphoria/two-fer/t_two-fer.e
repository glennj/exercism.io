include std/unittest.e

include two-fer.ex

set_test_verbosity(TEST_SHOW_ALL)

test_equal("no name given",two_fer(),"One for you, one for me.")
test_equal("a name given",two_fer("Alice"),"One for Alice, one for me.")
test_equal("another name given",two_fer("Bob"),"One for Bob, one for me.")

test_report()