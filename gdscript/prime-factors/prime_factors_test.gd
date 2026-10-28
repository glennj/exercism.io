func test_no_factors(solution_script):
	var want = []
	var got = solution_script.factors(1)
	return [got, want]


func test_prime_number(solution_script):
	var want = [2]
	var got = solution_script.factors(2)
	return [got, want]


func test_another_prime_number(solution_script):
	var want = [3]
	var got = solution_script.factors(3)
	return [got, want]


func test_square_of_a_prime(solution_script):
	var want = [3, 3]
	var got = solution_script.factors(9)
	return [got, want]


func test_product_of_first_prime(solution_script):
	var want = [2, 2]
	var got = solution_script.factors(4)
	return [got, want]


func test_cube_of_a_prime(solution_script):
	var want = [2, 2, 2]
	var got = solution_script.factors(8)
	return [got, want]


func test_product_of_second_prime(solution_script):
	var want = [3, 3, 3]
	var got = solution_script.factors(27)
	return [got, want]


func test_product_of_third_prime(solution_script):
	var want = [5, 5, 5, 5]
	var got = solution_script.factors(625)
	return [got, want]


func test_product_of_first_and_second_prime(solution_script):
	var want = [2, 3]
	var got = solution_script.factors(6)
	return [got, want]


func test_product_of_primes_and_non_primes(solution_script):
	var want = [2, 2, 3]
	var got = solution_script.factors(12)
	return [got, want]


func test_product_of_primes(solution_script):
	var want = [5, 17, 23, 461]
	var got = solution_script.factors(901255)
	return [got, want]


func test_factors_include_a_large_prime(solution_script):
	var want = [11, 9539, 894119]
	var got = solution_script.factors(93819012551)
	return [got, want]
