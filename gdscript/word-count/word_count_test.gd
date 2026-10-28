func test_count_one_word(solution_script):
	var got = solution_script.count_words("word")
	var want = {"word": 1}
	return [got, want]


func test_count_one_of_each_word(solution_script):
	var got = solution_script.count_words("one of each")
	var want = {"one": 1, "of": 1, "each": 1}
	return [got, want]


func test_multiple_occurrences_of_a_word(solution_script):
	var got = solution_script.count_words("one fish two fish red fish blue fish")
	var want = {"one": 1, "fish": 4, "two": 1, "red": 1, "blue": 1}
	return [got, want]


func test_handles_cramped_lists(solution_script):
	var got = solution_script.count_words("one,two,three")
	var want = {"one": 1, "two": 1, "three": 1}
	return [got, want]


func test_handles_expanded_lists(solution_script):
	var got = solution_script.count_words("one,\ntwo,\nthree")
	var want = {"one": 1, "two": 1, "three": 1}
	return [got, want]


func test_ignore_punctuation(solution_script):
	var got = solution_script.count_words("car: carpet as java: javascript!!&@$%^&")
	var want = {"car": 1, "carpet": 1, "as": 1, "java": 1, "javascript": 1}
	return [got, want]


func test_include_numbers(solution_script):
	var got = solution_script.count_words("testing, 1, 2 testing")
	var want = {"testing": 2, "1": 1, "2": 1}
	return [got, want]


func test_normalize_case(solution_script):
	var got = solution_script.count_words("go Go GO Stop stop")
	var want = {"go": 3, "stop": 2}
	return [got, want]


func test_with_apostrophes(solution_script):
	var got = solution_script.count_words("'First: don't laugh. Then: don't cry. You're getting it.'")
	var want = {"first": 1, "don't": 2, "laugh": 1, "then": 1, "cry": 1, "you're": 1, "getting": 1, "it": 1}
	return [got, want]


func test_with_quotations(solution_script):
	var got = solution_script.count_words("Joe can't tell between 'large' and large.")
	var want = {"joe": 1, "can't": 1, "tell": 1, "between": 1, "large": 2, "and": 1}
	return [got, want]


func test_substrings_from_the_beginning(solution_script):
	var got = solution_script.count_words("Joe can't tell between app, apple and a.")
	var want = {"joe": 1, "can't": 1, "tell": 1, "between": 1, "app": 1, "apple": 1, "and": 1, "a": 1}
	return [got, want]


func test_multiple_spaces_not_detected_as_a_word(solution_script):
	var got = solution_script.count_words(" multiple   whitespaces")
	var want = {"multiple": 1, "whitespaces": 1}
	return [got, want]


func test_alternating_word_separators_not_detected_as_a_word(solution_script):
	var got = solution_script.count_words(",\n,one,\n ,two \n 'three'")
	var want = {"one": 1, "two": 1, "three": 1}
	return [got, want]


func test_quotation_for_word_with_apostrophe(solution_script):
	var got = solution_script.count_words("can, can't, 'can't'")
	var want = {"can": 1, "can't": 2}
	return [got, want]
