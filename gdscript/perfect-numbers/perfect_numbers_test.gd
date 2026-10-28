func test_smallest_perfect_number_is_classified_correctly(solution_script):
	var got = solution_script.classify(6)
	var want = "perfect"
	return [got, want]


func test_medium_perfect_number_is_classified_correctly(solution_script):
	var got = solution_script.classify(28)
	var want = "perfect"
	return [got, want]


func test_large_perfect_number_is_classified_correctly(solution_script):
	var got = solution_script.classify(33550336)
	var want = "perfect"
	return [got, want]


func test_smallest_abundant_number_is_classified_correctly(solution_script):
	var got = solution_script.classify(12)
	var want = "abundant"
	return [got, want]


func test_medium_abundant_number_is_classified_correctly(solution_script):
	var got = solution_script.classify(30)
	var want = "abundant"
	return [got, want]


func test_large_abundant_number_is_classified_correctly(solution_script):
	var got = solution_script.classify(33550335)
	var want = "abundant"
	return [got, want]


func test_perfect_square_abundant_number_is_classified_correctly(solution_script):
	var got = solution_script.classify(196)
	var want = "abundant"
	return [got, want]


func test_smallest_prime_deficient_number_is_classified_correctly(solution_script):
	var got = solution_script.classify(2)
	var want = "deficient"
	return [got, want]


func test_smallest_non_prime_deficient_number_is_classified_correctly(solution_script):
	var got = solution_script.classify(4)
	var want = "deficient"
	return [got, want]


func test_medium_deficient_number_is_classified_correctly(solution_script):
	var got = solution_script.classify(32)
	var want = "deficient"
	return [got, want]


func test_large_deficient_number_is_classified_correctly(solution_script):
	var got = solution_script.classify(33550337)
	var want = "deficient"
	return [got, want]


func test_edge_case_no_factors_other_than_itself_is_classified_correctly(solution_script):
	var got = solution_script.classify(1)
	var want = "deficient"
	return [got, want]


func test_zero_is_rejected_as_it_is_not_a_positive_integer_(solution_script):
	var got = solution_script.classify(0)
	var want = null
	return [got, want]


func test_negative_integer_is_rejected_as_it_is_not_a_positive_integer_(solution_script):
	var got = solution_script.classify(-1)
	var want = null
	return [got, want]
