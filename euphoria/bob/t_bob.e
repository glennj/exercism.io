include std/unittest.e 

include bob.ex 

set_test_verbosity(TEST_SHOW_ALL)

test_equal("stating something",hey("Tom-ay-to, tom-aaaah-to."),"Whatever.")
test_equal("shouting",hey("WATCH OUT!"),"Whoa, chill out!")
test_equal("shouting gibberish",hey("FCECDFCAAB"),"Whoa, chill out!")
test_equal("asking a question",hey("Does this cryogenic chamber make me look fat?"),"Sure.")
test_equal("asking a numeric question",hey("You are, what, like 15?"),"Sure.")
test_equal("asking gibberish",hey("fffbbcbeab?"),"Sure.")
test_equal("talking forcefully",hey("Hi there!"),"Whatever.")
test_equal("using acronyms in regular speech",hey("It's OK if you don't want to go work for NASA."),"Whatever.")
test_equal("forceful question",hey("WHAT'S GOING ON?"),"Calm down, I know what I'm doing!")
test_equal("shouting numbers",hey("1, 2, 3 GO!"),"Whoa, chill out!")
test_equal("no letters",hey("1, 2, 3"),"Whatever.")
test_equal("question with no letters",hey("4?"),"Sure.")
test_equal("shouting with special characters",hey("ZOMG THE %^*@#$(*^ ZOMBIES ARE COMING!!11!!1!"),"Whoa, chill out!")
test_equal("shouting with no exclamation mark",hey("I HATE THE DENTIST"),"Whoa, chill out!")
test_equal("statement containing question mark",hey("Ending with ? means a question."),"Whatever.")
test_equal("non-letters with question",hey(":) ?"),"Sure.")
test_equal("prattling on",hey("Wait! Hang on. Are you going to be OK?"),"Sure.")
test_equal("silence",hey(""),"Fine. Be that way!")
test_equal("prolonged silence",hey("          "),"Fine. Be that way!")
test_equal("alternate silence",hey("\t\t\t\t\t\t\t\t\t\t"),"Fine. Be that way!")
test_equal("multiple line question",hey("\nDoes this cryogenic chamber make me look fat?\nNo."),"Whatever.")
test_equal("starting with whitespace",hey("         hmmmmmmm..."),"Whatever.")
test_equal("ending with whitespace",hey("Okay if like my  spacebar  quite a bit?   "),"Sure.")
test_equal("other whitespace",hey("\n\n \t"),"Fine. Be that way!")
test_equal("non-question ending with whitespace",hey("This is a statement ending with whitespace      "),"Whatever.")

test_report() 
