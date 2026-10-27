func test_no_matches(solution_script):
	var subject = "diaper"
	var candidates = ['hello', 'world', 'zombies', 'pants']
	var want = []
	var got = solution_script.find_anagram(subject, candidates)
	return [got, want]


func test_detects_two_anagrams(solution_script):
	var subject = "solemn"
	var candidates = ['lemons', 'cherry', 'melons']
	var want = ['lemons', 'melons']
	var got = solution_script.find_anagram(subject, candidates)
	return [got, want]


func test_does_not_detect_anagram_subsets(solution_script):
	var subject = "good"
	var candidates = ['dog', 'goody']
	var want = []
	var got = solution_script.find_anagram(subject, candidates)
	return [got, want]


func test_detects_anagram(solution_script):
	var subject = "listen"
	var candidates = ['enlists', 'google', 'inlets', 'banana']
	var want = ['inlets']
	var got = solution_script.find_anagram(subject, candidates)
	return [got, want]


func test_detects_three_anagrams(solution_script):
	var subject = "allergy"
	var candidates = ['gallery', 'ballerina', 'regally', 'clergy', 'largely', 'leading']
	var want = ['gallery', 'regally', 'largely']
	var got = solution_script.find_anagram(subject, candidates)
	return [got, want]


func test_detects_multiple_anagrams_with_different_case(solution_script):
	var subject = "nose"
	var candidates = ['Eons', 'ONES']
	var want = ['Eons', 'ONES']
	var got = solution_script.find_anagram(subject, candidates)
	return [got, want]


func test_does_not_detect_non_anagrams_with_identical_checksum(solution_script):
	var subject = "mass"
	var candidates = ['last']
	var want = []
	var got = solution_script.find_anagram(subject, candidates)
	return [got, want]


func test_detects_anagrams_case_insensitively(solution_script):
	var subject = "Orchestra"
	var candidates = ['cashregister', 'Carthorse', 'radishes']
	var want = ['Carthorse']
	var got = solution_script.find_anagram(subject, candidates)
	return [got, want]


func test_detects_anagrams_using_case_insensitive_subject(solution_script):
	var subject = "Orchestra"
	var candidates = ['cashregister', 'carthorse', 'radishes']
	var want = ['carthorse']
	var got = solution_script.find_anagram(subject, candidates)
	return [got, want]


func test_detects_anagrams_using_case_insensitive_possible_matches(solution_script):
	var subject = "orchestra"
	var candidates = ['cashregister', 'Carthorse', 'radishes']
	var want = ['Carthorse']
	var got = solution_script.find_anagram(subject, candidates)
	return [got, want]


func test_does_not_detect_an_anagram_if_the_original_word_is_repeated(solution_script):
	var subject = "go"
	var candidates = ['goGoGO']
	var want = []
	var got = solution_script.find_anagram(subject, candidates)
	return [got, want]


func test_anagrams_must_use_all_letters_exactly_once(solution_script):
	var subject = "tapper"
	var candidates = ['patter']
	var want = []
	var got = solution_script.find_anagram(subject, candidates)
	return [got, want]


func test_words_are_not_anagrams_of_themselves(solution_script):
	var subject = "BANANA"
	var candidates = ['BANANA']
	var want = []
	var got = solution_script.find_anagram(subject, candidates)
	return [got, want]


func test_words_are_not_anagrams_of_themselves_even_if_letter_case_is_partially_different(solution_script):
	var subject = "BANANA"
	var candidates = ['Banana']
	var want = []
	var got = solution_script.find_anagram(subject, candidates)
	return [got, want]


func test_words_are_not_anagrams_of_themselves_even_if_letter_case_is_completely_different(solution_script):
	var subject = "BANANA"
	var candidates = ['banana']
	var want = []
	var got = solution_script.find_anagram(subject, candidates)
	return [got, want]


func test_words_other_than_themselves_can_be_anagrams(solution_script):
	var subject = "LISTEN"
	var candidates = ['LISTEN', 'Silent']
	var want = ['Silent']
	var got = solution_script.find_anagram(subject, candidates)
	return [got, want]


func test_handles_case_of_greek_letters(solution_script):
	var subject = "ΑΒΓ"
	var candidates = ['ΒΓΑ', 'ΒΓΔ', 'γβα', 'αβγ']
	var want = ['ΒΓΑ', 'γβα']
	var got = solution_script.find_anagram(subject, candidates)
	return [got, want]


func test_different_characters_may_have_the_same_bytes(solution_script):
	var subject = "a⬂"
	var candidates = ['€a']
	var want = []
	var got = solution_script.find_anagram(subject, candidates)
	return [got, want]
