func test_basic(solution_script):
	var got = solution_script.abbreviate("Portable Network Graphics")
	var want = "PNG"
	return [got, want]


func test_lowercase_words(solution_script):
	var got = solution_script.abbreviate("Ruby on Rails")
	var want = "ROR"
	return [got, want]


func test_punctuation(solution_script):
	var got = solution_script.abbreviate("First In, First Out")
	var want = "FIFO"
	return [got, want]


func test_all_caps_word(solution_script):
	var got = solution_script.abbreviate("GNU Image Manipulation Program")
	var want = "GIMP"
	return [got, want]


func test_punctuation_without_whitespace(solution_script):
	var got = solution_script.abbreviate("Complementary metal-oxide semiconductor")
	var want = "CMOS"
	return [got, want]


func test_very_long_abbreviation(solution_script):
	var got = solution_script.abbreviate("Rolling On The Floor Laughing So Hard That My Dogs Came Over And Licked Me")
	var want = "ROTFLSHTMDCOALM"
	return [got, want]


func test_consecutive_delimiters(solution_script):
	var got = solution_script.abbreviate("Something - I made up from thin air")
	var want = "SIMUFTA"
	return [got, want]


func test_apostrophes(solution_script):
	var got = solution_script.abbreviate("Halley's Comet")
	var want = "HC"
	return [got, want]


func test_underscore_emphasis(solution_script):
	var got = solution_script.abbreviate("The Road _Not_ Taken")
	var want = "TRNT"
	return [got, want]
