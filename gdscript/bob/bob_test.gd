func test_asking_a_question(solution_script):
	var want = "Sure."
	var got = solution_script.response("Does this cryogenic chamber make me look fat?")
	return [got, want]


func test_shouting(solution_script):
	var want = "Whoa, chill out!"
	var got = solution_script.response("WATCH OUT!")
	return [got, want]


func test_forceful_question(solution_script):
	var want = "Calm down, I know what I'm doing!"
	var got = solution_script.response("WHAT'S GOING ON?")
	return [got, want]


func test_silence(solution_script):
	var want = "Fine. Be that way!"
	var got = solution_script.response("")
	return [got, want]


func test_stating_something(solution_script):
	var want = "Whatever."
	var got = solution_script.response("Tom-ay-to, tom-aaaah-to.")
	return [got, want]


func test_asking_a_numeric_question(solution_script):
	var want = "Sure."
	var got = solution_script.response("You are, what, like 15?")
	return [got, want]


func test_asking_gibberish(solution_script):
	var want = "Sure."
	var got = solution_script.response("fffbbcbeab?")
	return [got, want]


func test_question_with_no_letters(solution_script):
	var want = "Sure."
	var got = solution_script.response("4?")
	return [got, want]


func test_non_letters_with_question(solution_script):
	var want = "Sure."
	var got = solution_script.response(":) ?")
	return [got, want]


func test_prattling_on(solution_script):
	var want = "Sure."
	var got = solution_script.response("Wait! Hang on. Are you going to be OK?")
	return [got, want]


func test_ending_with_whitespace(solution_script):
	var want = "Sure."
	var got = solution_script.response("Okay if like my  spacebar  quite a bit?   ")
	return [got, want]


func test_multiple_line_question(solution_script):
	var want = "Sure."
	var got = solution_script.response("
Does this cryogenic chamber make
 me look fat?")
	return [got, want]


func test_shouting_gibberish(solution_script):
	var want = "Whoa, chill out!"
	var got = solution_script.response("FCECDFCAAB")
	return [got, want]


func test_shouting_a_statement_containing_a_question_mark(solution_script):
	var want = "Whoa, chill out!"
	var got = solution_script.response("DO LIONS EAT PEOPLE? AHHHHH.")
	return [got, want]


func test_shouting_numbers(solution_script):
	var want = "Whoa, chill out!"
	var got = solution_script.response("1, 2, 3 GO!")
	return [got, want]


func test_shouting_with_special_characters(solution_script):
	var want = "Whoa, chill out!"
	var got = solution_script.response("ZOMG THE %^*@#$(*^ ZOMBIES ARE COMING!!11!!1!")
	return [got, want]


func test_shouting_with_no_exclamation_mark(solution_script):
	var want = "Whoa, chill out!"
	var got = solution_script.response("I HATE THE DENTIST")
	return [got, want]


func test_prolonged_silence(solution_script):
	var want = "Fine. Be that way!"
	var got = solution_script.response("          ")
	return [got, want]


func test_alternate_silence(solution_script):
	var want = "Fine. Be that way!"
	var got = solution_script.response("										")
	return [got, want]


func test_other_whitespace(solution_script):
	var want = "Fine. Be that way!"
	var got = solution_script.response("
 	")
	return [got, want]


func test_talking_forcefully(solution_script):
	var want = "Whatever."
	var got = solution_script.response("Hi there!")
	return [got, want]


func test_using_acronyms_in_regular_speech(solution_script):
	var want = "Whatever."
	var got = solution_script.response("It's OK if you don't want to go work for NASA.")
	return [got, want]


func test_no_letters(solution_script):
	var want = "Whatever."
	var got = solution_script.response("1, 2, 3")
	return [got, want]


func test_statement_containing_question_mark(solution_script):
	var want = "Whatever."
	var got = solution_script.response("Ending with ? means a question.")
	return [got, want]


func test_starting_with_whitespace(solution_script):
	var want = "Whatever."
	var got = solution_script.response("         hmmmmmmm...")
	return [got, want]


func test_non_question_ending_with_whitespace(solution_script):
	var want = "Whatever."
	var got = solution_script.response("This is a statement ending with whitespace      ")
	return [got, want]
