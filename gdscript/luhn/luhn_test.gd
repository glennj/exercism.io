func test_single_digit_strings_can_not_be_valid(solution_script):
	solution_script.card_num = "1"
	var got = solution_script.valid()
	var want = false
	return [got, want]


func test_a_single_zero_is_invalid(solution_script):
	solution_script.card_num = "0"
	var got = solution_script.valid()
	var want = false
	return [got, want]


func test_a_simple_valid_sin_that_remains_valid_if_reversed(solution_script):
	solution_script.card_num = "059"
	var got = solution_script.valid()
	var want = true
	return [got, want]


func test_a_simple_valid_sin_that_becomes_invalid_if_reversed(solution_script):
	solution_script.card_num = "59"
	var got = solution_script.valid()
	var want = true
	return [got, want]


func test_a_valid_canadian_sin(solution_script):
	solution_script.card_num = "055 444 285"
	var got = solution_script.valid()
	var want = true
	return [got, want]


func test_invalid_canadian_sin(solution_script):
	solution_script.card_num = "055 444 286"
	var got = solution_script.valid()
	var want = false
	return [got, want]


func test_invalid_credit_card(solution_script):
	solution_script.card_num = "8273 1232 7352 0569"
	var got = solution_script.valid()
	var want = false
	return [got, want]


func test_invalid_long_number_with_an_even_remainder(solution_script):
	solution_script.card_num = "1 2345 6789 1234 5678 9012"
	var got = solution_script.valid()
	var want = false
	return [got, want]


func test_invalid_long_number_with_a_remainder_divisible_by_5(solution_script):
	solution_script.card_num = "1 2345 6789 1234 5678 9013"
	var got = solution_script.valid()
	var want = false
	return [got, want]


func test_valid_number_with_an_even_number_of_digits(solution_script):
	solution_script.card_num = "095 245 88"
	var got = solution_script.valid()
	var want = true
	return [got, want]


func test_valid_number_with_an_odd_number_of_spaces(solution_script):
	solution_script.card_num = "234 567 891 234"
	var got = solution_script.valid()
	var want = true
	return [got, want]


func test_valid_strings_with_a_non_digit_added_at_the_end_become_invalid(solution_script):
	solution_script.card_num = "059a"
	var got = solution_script.valid()
	var want = false
	return [got, want]


func test_valid_strings_with_punctuation_included_become_invalid(solution_script):
	solution_script.card_num = "055-444-285"
	var got = solution_script.valid()
	var want = false
	return [got, want]


func test_valid_strings_with_symbols_included_become_invalid(solution_script):
	solution_script.card_num = "055# 444$ 285"
	var got = solution_script.valid()
	var want = false
	return [got, want]


func test_single_zero_with_space_is_invalid(solution_script):
	solution_script.card_num = " 0"
	var got = solution_script.valid()
	var want = false
	return [got, want]


func test_more_than_a_single_zero_is_valid(solution_script):
	solution_script.card_num = "0000 0"
	var got = solution_script.valid()
	var want = true
	return [got, want]


func test_input_digit_9_is_correctly_converted_to_output_digit_9(solution_script):
	solution_script.card_num = "091"
	var got = solution_script.valid()
	var want = true
	return [got, want]


func test_very_long_input_is_valid(solution_script):
	solution_script.card_num = "9999999999 9999999999 9999999999 9999999999"
	var got = solution_script.valid()
	var want = true
	return [got, want]


func test_valid_luhn_with_an_odd_number_of_digits_and_non_zero_first_digit(solution_script):
	solution_script.card_num = "109"
	var got = solution_script.valid()
	var want = true
	return [got, want]


func test_using_ascii_value_for_non_doubled_non_digit_isn_t_allowed(solution_script):
	solution_script.card_num = "055b 444 285"
	var got = solution_script.valid()
	var want = false
	return [got, want]


func test_using_ascii_value_for_doubled_non_digit_isn_t_allowed(solution_script):
	solution_script.card_num = ":9"
	var got = solution_script.valid()
	var want = false
	return [got, want]


func test_non_numeric_non_space_char_in_the_middle_with_a_sum_that_s_divisible_by_10_isn_t_allowed(solution_script):
	solution_script.card_num = "59%59"
	var got = solution_script.valid()
	var want = false
	return [got, want]


func test_is_valid_can_be_called_repeatedly(solution_script):
	# This test was added, because we saw many implementations
	# in which the first call to valid() worked, but the
	# second call failed.
	solution_script.card_num = "055 444 285"
	return [solution_script.valid(), true]
	return [solution_script.valid(), true]
