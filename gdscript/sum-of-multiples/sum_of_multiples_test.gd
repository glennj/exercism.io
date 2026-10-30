func test_no_multiples_within_limit(solution_script):
	var got = solution_script.sum([3, 5], 1)
	var want = 0
	return [got, want]


func test_one_factor_has_multiples_within_limit(solution_script):
	var got = solution_script.sum([3, 5], 4)
	var want = 3
	return [got, want]


func test_more_than_one_multiple_within_limit(solution_script):
	var got = solution_script.sum([3], 7)
	var want = 9
	return [got, want]


func test_more_than_one_factor_with_multiples_within_limit(solution_script):
	var got = solution_script.sum([3, 5], 10)
	var want = 23
	return [got, want]


func test_each_multiple_is_only_counted_once(solution_script):
	var got = solution_script.sum([3, 5], 100)
	var want = 2318
	return [got, want]


func test_a_much_larger_limit(solution_script):
	var got = solution_script.sum([3, 5], 1000)
	var want = 233168
	return [got, want]


func test_three_factors(solution_script):
	var got = solution_script.sum([7, 13, 17], 20)
	var want = 51
	return [got, want]


func test_factors_not_relatively_prime(solution_script):
	var got = solution_script.sum([4, 6], 15)
	var want = 30
	return [got, want]


func test_some_pairs_of_factors_relatively_prime_and_some_not(solution_script):
	var got = solution_script.sum([5, 6, 8], 150)
	var want = 4419
	return [got, want]


func test_one_factor_is_a_multiple_of_another(solution_script):
	var got = solution_script.sum([5, 25], 51)
	var want = 275
	return [got, want]


func test_much_larger_factors(solution_script):
	var got = solution_script.sum([43, 47], 10000)
	var want = 2203160
	return [got, want]


func test_all_numbers_are_multiples_of_1(solution_script):
	var got = solution_script.sum([1], 100)
	var want = 4950
	return [got, want]


func test_no_factors_means_an_empty_sum(solution_script):
	var got = solution_script.sum([], 10000)
	var want = 0
	return [got, want]


func test_the_only_multiple_of_0_is_0(solution_script):
	var got = solution_script.sum([0], 1)
	var want = 0
	return [got, want]


func test_the_factor_0_does_not_affect_the_sum_of_multiples_of_other_factors(solution_script):
	var got = solution_script.sum([3, 0], 4)
	var want = 3
	return [got, want]


func test_solutions_using_include_exclude_must_extend_to_cardinality_greater_than_3(solution_script):
	var got = solution_script.sum([2, 3, 5, 7, 11], 10000)
	var want = 39614537
	return [got, want]
