func test_puzzle_with_three_letters(solution_script):
	var got = solution_script.solve("I + BB == ILL")
	var want = {"I": 1, "B": 9, "L": 0}
	return [got, want]


func test_solution_must_have_unique_value_for_each_letter(solution_script):
	var got = solution_script.solve("A == B")
	var want = null
	return [got, want]


func test_leading_zero_solution_is_invalid(solution_script):
	var got = solution_script.solve("ACA + DD == BD")
	var want = null
	return [got, want]


func test_puzzle_with_two_digits_final_carry(solution_script):
	var got = solution_script.solve("A + A + A + A + A + A + A + A + A + A + A + B == BCC")
	var want = {"A": 9, "B": 1, "C": 0}
	return [got, want]


func test_puzzle_with_four_letters(solution_script):
	var got = solution_script.solve("AS + A == MOM")
	var want = {"A": 9, "S": 2, "M": 1, "O": 0}
	return [got, want]


func test_puzzle_with_six_letters(solution_script):
	var got = solution_script.solve("NO + NO + TOO == LATE")
	var want = {"N": 7, "O": 4, "T": 9, "L": 1, "A": 0, "E": 2}
	return [got, want]


func test_puzzle_with_seven_letters(solution_script):
	var got = solution_script.solve("HE + SEES + THE == LIGHT")
	var want = {"E": 4, "G": 2, "H": 5, "I": 0, "L": 1, "S": 9, "T": 7}
	return [got, want]


func test_puzzle_with_eight_letters(solution_script):
	var got = solution_script.solve("SEND + MORE == MONEY")
	var want = {"S": 9, "E": 5, "N": 6, "D": 7, "M": 1, "O": 0, "R": 8, "Y": 2}
	return [got, want]
