func test_initially_9_failures_are_allowed_and_no_letters_are_guessed__state(solution_script):
	var game = solution_script.SaveTheCow.new("loot")
	for letter in []:
		game.guess(letter)
		if game.error != null:
			return [game.error, null]
	return [game.state, "Ongoing"]


func test_initially_9_failures_are_allowed_and_no_letters_are_guessed__masked_word(solution_script):
	var game = solution_script.SaveTheCow.new("loot")
	for letter in []:
		game.guess(letter)
		if game.error != null:
			return [game.error, null]
	return [game.masked_word, "____"]


func test_initially_9_failures_are_allowed_and_no_letters_are_guessed__remaining_failures(solution_script):
	var game = solution_script.SaveTheCow.new("loot")
	for letter in []:
		game.guess(letter)
		if game.error != null:
			return [game.error, null]
	return [game.remaining_failures, 9]


func test_after_10_failures_the_game_is_over__state(solution_script):
	var game = solution_script.SaveTheCow.new("loot")
	for letter in ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"]:
		game.guess(letter)
		if game.error != null:
			return [game.error, null]
	return [game.state, "Lose"]


func test_after_10_failures_the_game_is_over__masked_word(solution_script):
	var game = solution_script.SaveTheCow.new("loot")
	for letter in ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"]:
		game.guess(letter)
		if game.error != null:
			return [game.error, null]
	return [game.masked_word, "____"]


func test_after_10_failures_the_game_is_over__remaining_failures(solution_script):
	var game = solution_script.SaveTheCow.new("loot")
	for letter in ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"]:
		game.guess(letter)
		if game.error != null:
			return [game.error, null]
	return [game.remaining_failures, 0]


func test_losing_with_several_correct_guesses__state(solution_script):
	var game = solution_script.SaveTheCow.new("loot")
	for letter in ["t", "o", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j"]:
		game.guess(letter)
		if game.error != null:
			return [game.error, null]
	return [game.state, "Lose"]


func test_losing_with_several_correct_guesses__masked_word(solution_script):
	var game = solution_script.SaveTheCow.new("loot")
	for letter in ["t", "o", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j"]:
		game.guess(letter)
		if game.error != null:
			return [game.error, null]
	return [game.masked_word, "_oot"]


func test_losing_with_several_correct_guesses__remaining_failures(solution_script):
	var game = solution_script.SaveTheCow.new("loot")
	for letter in ["t", "o", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j"]:
		game.guess(letter)
		if game.error != null:
			return [game.error, null]
	return [game.remaining_failures, 0]


func test_feeding_a_correct_letter_removes_underscores__state(solution_script):
	var game = solution_script.SaveTheCow.new("loot")
	for letter in ["t"]:
		game.guess(letter)
		if game.error != null:
			return [game.error, null]
	return [game.state, "Ongoing"]


func test_feeding_a_correct_letter_removes_underscores__masked_word(solution_script):
	var game = solution_script.SaveTheCow.new("loot")
	for letter in ["t"]:
		game.guess(letter)
		if game.error != null:
			return [game.error, null]
	return [game.masked_word, "___t"]


func test_feeding_a_correct_letter_removes_underscores__remaining_failures(solution_script):
	var game = solution_script.SaveTheCow.new("loot")
	for letter in ["t"]:
		game.guess(letter)
		if game.error != null:
			return [game.error, null]
	return [game.remaining_failures, 9]


func test_feeding_a_correct_letter_twice_counts_as_a_failure__state(solution_script):
	var game = solution_script.SaveTheCow.new("loot")
	for letter in ["t", "t"]:
		game.guess(letter)
		if game.error != null:
			return [game.error, null]
	return [game.state, "Ongoing"]


func test_feeding_a_correct_letter_twice_counts_as_a_failure__masked_word(solution_script):
	var game = solution_script.SaveTheCow.new("loot")
	for letter in ["t", "t"]:
		game.guess(letter)
		if game.error != null:
			return [game.error, null]
	return [game.masked_word, "___t"]


func test_feeding_a_correct_letter_twice_counts_as_a_failure__remaining_failures(solution_script):
	var game = solution_script.SaveTheCow.new("loot")
	for letter in ["t", "t"]:
		game.guess(letter)
		if game.error != null:
			return [game.error, null]
	return [game.remaining_failures, 8]


func test_guessing_a_repeated_letter_reveals_all_instances__state(solution_script):
	var game = solution_script.SaveTheCow.new("loot")
	for letter in ["t", "t", "o"]:
		game.guess(letter)
		if game.error != null:
			return [game.error, null]
	return [game.state, "Ongoing"]


func test_guessing_a_repeated_letter_reveals_all_instances__masked_word(solution_script):
	var game = solution_script.SaveTheCow.new("loot")
	for letter in ["t", "t", "o"]:
		game.guess(letter)
		if game.error != null:
			return [game.error, null]
	return [game.masked_word, "_oot"]


func test_guessing_a_repeated_letter_reveals_all_instances__remaining_failures(solution_script):
	var game = solution_script.SaveTheCow.new("loot")
	for letter in ["t", "t", "o"]:
		game.guess(letter)
		if game.error != null:
			return [game.error, null]
	return [game.remaining_failures, 8]


func test_getting_all_the_letters_right_makes_for_a_win__state(solution_script):
	var game = solution_script.SaveTheCow.new("loot")
	for letter in ["t", "t", "o", "l"]:
		game.guess(letter)
		if game.error != null:
			return [game.error, null]
	return [game.state, "Win"]


func test_getting_all_the_letters_right_makes_for_a_win__masked_word(solution_script):
	var game = solution_script.SaveTheCow.new("loot")
	for letter in ["t", "t", "o", "l"]:
		game.guess(letter)
		if game.error != null:
			return [game.error, null]
	return [game.masked_word, "loot"]


func test_getting_all_the_letters_right_makes_for_a_win__remaining_failures(solution_script):
	var game = solution_script.SaveTheCow.new("loot")
	for letter in ["t", "t", "o", "l"]:
		game.guess(letter)
		if game.error != null:
			return [game.error, null]
	return [game.remaining_failures, 8]


func test_winning_on_the_last_guess_is_still_a_win__state(solution_script):
	var game = solution_script.SaveTheCow.new("loot")
	for letter in ["a", "b", "c", "d", "e", "f", "g", "h", "i", "t", "o", "l"]:
		game.guess(letter)
		if game.error != null:
			return [game.error, null]
	return [game.state, "Win"]


func test_winning_on_the_last_guess_is_still_a_win__masked_word(solution_script):
	var game = solution_script.SaveTheCow.new("loot")
	for letter in ["a", "b", "c", "d", "e", "f", "g", "h", "i", "t", "o", "l"]:
		game.guess(letter)
		if game.error != null:
			return [game.error, null]
	return [game.masked_word, "loot"]


func test_winning_on_the_last_guess_is_still_a_win__remaining_failures(solution_script):
	var game = solution_script.SaveTheCow.new("loot")
	for letter in ["a", "b", "c", "d", "e", "f", "g", "h", "i", "t", "o", "l"]:
		game.guess(letter)
		if game.error != null:
			return [game.error, null]
	return [game.remaining_failures, 0]


func test_guessing_after_a_lose_is_error__error(solution_script):
	var game = solution_script.SaveTheCow.new("loot")
	for letter in ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"]:
		game.guess(letter)
		if game.error != null:
			return [game.error, null]
	game.guess("k")
	return [game.error, "cannot guess after the game is lost"]


func test_guessing_after_a_win_is_error__error(solution_script):
	var game = solution_script.SaveTheCow.new("loot")
	for letter in ["t", "o", "l"]:
		game.guess(letter)
		if game.error != null:
			return [game.error, null]
	game.guess("l")
	return [game.error, "cannot guess after the game is won"]
