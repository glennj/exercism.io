include std/unittest.e 

include hamming.ex 

set_test_verbosity(TEST_SHOW_ALL)

test_equal("empty strands",distance("",""),0)
test_equal("single letter identical strands",distance("A","A"),0)
test_equal("single letter different strands",distance("G","T"),1)
test_equal("long identical strands",distance("GGACTGAAATCTG","GGACTGAAATCTG"),0)
test_equal("long different strands",distance("GGACGGATTCTG","AGGACGGATTCT"),9)
test_equal("disallow first strand longer",distance("AATG","AAA"),"left and right strands must be of equal length")
test_equal("disallow left empty strand",distance("","G"),"left and right strands must be of equal length")
test_equal("disallow right empty strand",distance("G",""),"left and right strands must be of equal length")

test_report() 