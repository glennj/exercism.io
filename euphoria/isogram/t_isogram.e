include std/unittest.e 

include isogram.ex 

set_test_verbosity(TEST_SHOW_ALL)

test_true("empty string" , isogram("")  )
test_true("isogram with only lower case characters" , isogram("isogram") ) 
test_false("word with one duplicated character" , isogram("eleven")  )
test_false("word with one duplicated character from the end of the alphabet" , isogram("zzyzx") ) 
test_true("longest reported english isogram" , isogram("subdermatoglyphic")  )
test_false("word with duplicated character in mixed case" , isogram("Alphabet") ) 
test_false("word with duplicated character in mixed case, lowercase first" , isogram("alphAbet"))  
test_true("hypothetical isogrammic word with hyphen" , isogram("thumbscrew-japingly")  )
test_false("hypothetical word with duplicated character following hyphen" , isogram("thumbscrew-jappingly"))  
test_true("isogram with duplicated hyphen" , isogram("six-year-old")  )
test_true("made-up name that is an isogram" , isogram("Emily Jung Schwartzkopf"))  
test_false("duplicated character in the middle" , isogram("accentor")  )
test_false("same first and last characters" , isogram("angola")  )
test_false("word with duplicated character and with two hyphens" , isogram("up-to-date"))  
 
test_report() 
