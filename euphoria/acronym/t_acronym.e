include std/unittest.e 

include acronym.ex 

set_test_verbosity(TEST_SHOW_ALL)

test_equal("basic","PNG",acronym("Portable Network Graphics"))
test_equal("lowercase words","ROR",acronym("Ruby on Rails"))
test_equal("punctuation","FIFO",acronym("First In, First Out"))
test_equal("all caps word","GIMP",acronym("GNU Image Manipulation Program"))
test_equal("punctuation without whitespace","CMOS",acronym("Complementary metal-oxide semiconductor"))
test_equal("very long abbreviation","ROTFLSHTMDCOALM",acronym("Rolling On The Floor Laughing So Hard That My Dogs Came Over And Licked Me"))
test_equal("consecutive delimiters","SIMUFTA",acronym("Something - I made up from thin air"))
test_equal("apostrophes","HC",acronym("Halley's Comet"))
test_equal("underscore emphasis","TRNT",acronym("The Road _Not_ Taken"))

test_report() 
