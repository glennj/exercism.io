include std/unittest.e 

include bob.ex 

set_test_verbosity(TEST_SHOW_ALL)

test_equal("stating something", "Whatever.", hey("Tom-ay-to, tom-aaaah-to."))
test_equal("shouting", "Whoa, chill out!", hey("WATCH OUT!"))
test_equal("shouting gibberish", "Whoa, chill out!", hey("FCECDFCAAB"))
test_equal("asking a question", "Sure.", hey("Does this cryogenic chamber make me look fat?"))
test_equal("asking a numeric question", "Sure.", hey("You are, what, like 15?"))
test_equal("asking gibberish", "Sure.", hey("fffbbcbeab?"))
test_equal("talking forcefully", "Whatever.", hey("Hi there!"))
test_equal("using acronyms in regular speech", "Whatever.", hey("It's OK if you don't want to go work for NASA."))
test_equal("forceful question", "Calm down, I know what I'm doing!", hey("WHAT'S GOING ON?"))
test_equal("shouting numbers", "Whoa, chill out!", hey("1, 2, 3 GO!"))
test_equal("no letters", "Whatever.", hey("1, 2, 3"))
test_equal("question with no letters", "Sure.", hey("4?"))
test_equal("shouting with special characters", "Whoa, chill out!", hey("ZOMG THE %^*@#$(*^ ZOMBIES ARE COMING!!11!!1!"))
test_equal("shouting with no exclamation mark", "Whoa, chill out!", hey("I HATE THE DENTIST"))
test_equal("statement containing question mark", "Whatever.", hey("Ending with ? means a question."))
test_equal("non-letters with question", hey(":) ?"),"Sure.")
test_equal("prattling on", "Sure.", hey("Wait! Hang on. Are you going to be OK?"))
test_equal("silence", "Fine. Be that way!", hey(""))
test_equal("prolonged silence", "Fine. Be that way!", hey("          "))
test_equal("alternate silence", "Fine. Be that way!", hey("\t\t\t\t\t\t\t\t\t\t"))
test_equal("multiple line question", "Whatever.", hey("\nDoes this cryogenic chamber make me look fat?\nNo."))
test_equal("starting with whitespace", "Whatever.", hey("         hmmmmmmm..."))
test_equal("ending with whitespace", "Sure.", hey("Okay if like my  spacebar  quite a bit?   "))
test_equal("other whitespace", "Fine. Be that way!", hey("\n\n \t"))
test_equal("non-question ending with whitespace", "Whatever.", hey("This is a statement ending with whitespace      "))

test_report() 
