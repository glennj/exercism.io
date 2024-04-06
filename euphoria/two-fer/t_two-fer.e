include std/unittest.e

include two-fer.ex

set_test_verbosity(TEST_SHOW_ALL)

procedure test_aquel(sequence name, object outcome, object expected) 
    test_equal(name,expected,outcome)
end procedure
test_aquel("no name given",two_fer(),"One for you, one for me.")
test_aquel("a name given",two_fer("Alice"),"One for Alice, one for me.")
test_aquel("another name given",two_fer("Bob"),"One for Bob, one for me.")

test_report()
