include std/unittest.e 

include anagram.ex 

set_test_verbosity(TEST_SHOW_ALL)

test_equal("no matches",findAnagrams("diaper",{"hello","world","zombies","pants"}),{})
test_equal("detects two anagrams",findAnagrams("master",{"stream","pigeon","maters"}),{"maters","stream"})
test_equal("detects two anagrams",findAnagrams("solemn",{"lemons","cherry","melons"}),{"lemons","melons"})
test_equal("does not detect anagram subsets",findAnagrams("good",{"dog","goody"}),{})
test_equal("detects anagram",findAnagrams("listen",{"enlists","google","inlets","banana"}),{"inlets"})
test_equal("detects three anagrams",findAnagrams("allergy",{"gallery","ballerina","regally","clergy","largely","leading"}),{"gallery","largely","regally"})
test_equal("detects multiple anagrams with different case",findAnagrams("nose",{"Eons","ONES"}),{"Eons","ONES"})
test_equal("does not detect non-anagrams with identical checksum",findAnagrams("mass",{"last"}),{})
test_equal("detects anagrams case-insensitively",findAnagrams("Orchestra",{"cashregister","Carthorse","radishes"}),{"Carthorse"})
test_equal("detects anagrams using case-insensitive subject",findAnagrams("Orchestra",{"cashregister","carthorse","radishes"}),{"carthorse"})
test_equal("detects anagrams using case-insensitive possible matches",findAnagrams("orchestra",{"cashregister","Carthorse","radishes"}),{"Carthorse"})
test_equal("does not detect an anagram if the original word is repeated",findAnagrams("go",{"go Go GO"}),{})
test_equal("anagrams must use all letters exactly once",findAnagrams("tapper",{"patter"}),{})
test_equal("words are not anagrams of themselves (case-insensitive)",findAnagrams("BANANA",{"BANANA","Banana","banana"}),{})
test_equal("words are not anagrams of themselves",findAnagrams("BANANA",{"BANANA"}),{})
test_equal("words are not anagrams of themselves even if letter case is partially different",findAnagrams("BANANA",{"Banana"}),{})
test_equal("words are not anagrams of themselves even if letter case is completely different",findAnagrams("BANANA",{"banana"}),{})
test_equal("words other than themselves can be anagrams",findAnagrams("LISTEN",{"Listen","Silent","LISTEN"}),{"Silent"})
test_equal("words other than themselves can be anagrams",findAnagrams("LISTEN",{"LISTEN","Silent"}),{"Silent"})

test_report() 