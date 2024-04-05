include std/unittest.e 

include hello-world.ex 

set_test_verbosity(TEST_SHOW_ALL)

test_equal("hello_world", "Hello, World!", hello_world()) 
 
test_report() 