func test_lowercase_letter(solution_script):
	var got = solution_script.score("a")
	var want = 1
	return [got, want]


func test_uppercase_letter(solution_script):
	var got = solution_script.score("A")
	var want = 1
	return [got, want]


func test_valuable_letter(solution_script):
	var got = solution_script.score("f")
	var want = 4
	return [got, want]


func test_short_word(solution_script):
	var got = solution_script.score("at")
	var want = 2
	return [got, want]


func test_short_valuable_word(solution_script):
	var got = solution_script.score("zoo")
	var want = 12
	return [got, want]


func test_medium_word(solution_script):
	var got = solution_script.score("street")
	var want = 6
	return [got, want]


func test_medium_valuable_word(solution_script):
	var got = solution_script.score("quirky")
	var want = 22
	return [got, want]


func test_long_mixed_case_word(solution_script):
	var got = solution_script.score("OxyphenButazone")
	var want = 41
	return [got, want]


func test_english_like_word(solution_script):
	var got = solution_script.score("pinata")
	var want = 8
	return [got, want]


func test_empty_input(solution_script):
	var got = solution_script.score("")
	var want = 0
	return [got, want]


func test_entire_alphabet_available(solution_script):
	var got = solution_script.score("abcdefghijklmnopqrstuvwxyz")
	var want = 87
	return [got, want]
