include std/unittest.e 

include acronym.ex 

set_test_verbosity(TEST_SHOW_ALL)

test_equal("basic",acronym("Portable Network Graphics"),"PNG")
test_equal("lowercase words",acronym("Ruby on Rails"),"ROR")
test_equal("punctuation",acronym("First In, First Out"),"FIFO")
test_equal("all caps word",acronym("GNU Image Manipulation Program"),"GIMP")
test_equal("punctuation without whitespace",acronym("Complementary metal-oxide semiconductor"),"CMOS")
test_equal("very long abbreviation",acronym("Rolling On The Floor Laughing So Hard That My Dogs Came Over And Licked Me"),"ROTFLSHTMDCOALM")
test_equal("consecutive delimiters",acronym("Something - I made up from thin air"),"SIMUFTA")
test_equal("apostrophes",acronym("Halley's Comet"),"HC")
test_equal("underscore emphasis",acronym("The Road _Not_ Taken"),"TRNT")

test_report() 
