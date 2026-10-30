func test_two_cards_one_trick(solution_script):
	var want = {"status": "finished", "cards": 2, "tricks": 1}
	var got = solution_script.simulate_game(
		["2"],
		["3"]
	)
	return [got, want]


func test_three_cards_one_trick(solution_script):
	var want = {"status": "finished", "cards": 3, "tricks": 1}
	var got = solution_script.simulate_game(
		["2", "4"],
		["3"]
	)
	return [got, want]


func test_four_cards_one_trick(solution_script):
	var want = {"status": "finished", "cards": 4, "tricks": 1}
	var got = solution_script.simulate_game(
		["2", "4"],
		["3", "5", "6"]
	)
	return [got, want]


func test_the_ace_reigns_supreme(solution_script):
	var want = {"status": "finished", "cards": 7, "tricks": 1}
	var got = solution_script.simulate_game(
		["2", "A"],
		["3", "4", "5", "6", "7"]
	)
	return [got, want]


func test_the_king_beats_ace(solution_script):
	var want = {"status": "finished", "cards": 7, "tricks": 1}
	var got = solution_script.simulate_game(
		["2", "A"],
		["3", "4", "5", "6", "K"]
	)
	return [got, want]


func test_the_queen_seduces_the_king(solution_script):
	var want = {"status": "finished", "cards": 10, "tricks": 1}
	var got = solution_script.simulate_game(
		["2", "A", "7", "8", "Q"],
		["3", "4", "5", "6", "K"]
	)
	return [got, want]


func test_the_jack_betrays_the_queen(solution_script):
	var want = {"status": "finished", "cards": 12, "tricks": 1}
	var got = solution_script.simulate_game(
		["2", "A", "7", "8", "Q"],
		["3", "4", "5", "6", "K", "9", "J"]
	)
	return [got, want]


func test_the_10_just_wants_to_put_on_a_show(solution_script):
	var want = {"status": "finished", "cards": 13, "tricks": 1}
	var got = solution_script.simulate_game(
		["2", "A", "7", "8", "Q", "10"],
		["3", "4", "5", "6", "K", "9", "J"]
	)
	return [got, want]


func test_simple_loop_with_decks_of_3_cards(solution_script):
	var want = {"status": "loop", "cards": 8, "tricks": 3}
	var got = solution_script.simulate_game(
		["J", "2", "3"],
		["4", "J", "5"]
	)
	return [got, want]


func test_the_story_is_starting_to_get_a_bit_complicated(solution_script):
	var want = {"status": "finished", "cards": 361, "tricks": 1}
	var got = solution_script.simulate_game(
		["2", "6", "6", "J", "4", "K", "Q", "10", "K", "J", "Q", "2", "3", "K", "5", "6", "Q", "Q", "A", "A", "6", "9", "K", "A", "8", "K", "2", "A", "9", "A", "Q", "4", "K", "K", "K", "3", "5", "K", "8", "Q", "3", "Q", "7", "J", "K", "J", "9", "J", "3", "3", "K", "K", "Q", "A", "K", "7", "10", "A", "Q", "7", "10", "J", "4", "5", "J", "9", "10", "Q", "J", "J", "K", "6", "10", "J", "6", "Q", "J", "5", "J", "Q", "Q", "8", "3", "8", "A", "2", "6", "9", "K", "7", "J", "K", "K", "8", "K", "Q", "6", "10", "J", "10", "J", "Q", "J", "10", "3", "8", "K", "A", "6", "9", "K", "2", "A", "A", "10", "J", "6", "A", "4", "J", "A", "J", "J", "6", "2", "J", "3", "K", "2", "5", "9", "J", "9", "6", "K", "A", "5", "Q", "J", "2", "Q", "K", "A", "3", "K", "J", "K", "2", "5", "6", "Q", "J", "Q", "Q", "J", "2", "J", "9", "Q", "7", "7", "A", "Q", "7", "Q", "J", "K", "J", "A", "7", "7", "8", "Q", "10", "J", "10", "J", "J", "9", "2", "A", "2"],
		["7", "2", "10", "K", "8", "2", "J", "9", "A", "5", "6", "J", "Q", "6", "K", "6", "5", "A", "4", "Q", "7", "J", "7", "10", "2", "Q", "8", "2", "2", "K", "J", "A", "5", "5", "A", "4", "Q", "6", "Q", "K", "10", "8", "Q", "2", "10", "J", "A", "Q", "8", "Q", "Q", "J", "J", "A", "A", "9", "10", "J", "K", "4", "Q", "10", "10", "J", "K", "10", "2", "J", "7", "A", "K", "K", "J", "A", "J", "10", "8", "K", "A", "7", "Q", "Q", "J", "3", "Q", "4", "A", "3", "A", "Q", "Q", "Q", "5", "4", "K", "J", "10", "A", "Q", "J", "6", "J", "A", "10", "A", "5", "8", "3", "K", "5", "9", "Q", "8", "7", "7", "J", "7", "Q", "Q", "Q", "A", "7", "8", "9", "A", "Q", "A", "K", "8", "A", "A", "J", "8", "4", "8", "K", "J", "A", "10", "Q", "8", "J", "8", "6", "10", "Q", "J", "J", "A", "A", "J", "5", "Q", "6", "J", "K", "Q", "8", "K", "4", "Q", "Q", "6", "J", "K", "4", "7", "J", "J", "9", "9", "A", "Q", "Q", "K", "A", "6", "5", "K"]
	)
	return [got, want]


func test_two_tricks(solution_script):
	var want = {"status": "finished", "cards": 5, "tricks": 2}
	var got = solution_script.simulate_game(
		["J"],
		["3", "J"]
	)
	return [got, want]


func test_more_tricks(solution_script):
	var want = {"status": "finished", "cards": 12, "tricks": 4}
	var got = solution_script.simulate_game(
		["J", "2", "4"],
		["3", "J", "A"]
	)
	return [got, want]


func test_simple_loop_with_decks_of_4_cards(solution_script):
	var want = {"status": "loop", "cards": 16, "tricks": 4}
	var got = solution_script.simulate_game(
		["2", "3", "J", "6"],
		["K", "5", "J", "7"]
	)
	return [got, want]


func test_easy_card_combination(solution_script):
	var want = {"status": "finished", "cards": 40, "tricks": 4}
	var got = solution_script.simulate_game(
		["4", "8", "7", "5", "4", "10", "3", "9", "7", "3", "10", "10", "6", "8", "2", "8", "5", "4", "5", "9", "6", "5", "2", "8", "10", "9"],
		["6", "9", "4", "7", "2", "2", "3", "6", "7", "3", "A", "A", "A", "A", "K", "K", "K", "K", "Q", "Q", "Q", "Q", "J", "J", "J", "J"]
	)
	return [got, want]


func test_easy_card_combination_inverted_decks(solution_script):
	var want = {"status": "finished", "cards": 40, "tricks": 4}
	var got = solution_script.simulate_game(
		["3", "3", "5", "7", "3", "2", "10", "7", "6", "7", "A", "A", "A", "A", "K", "K", "K", "K", "Q", "Q", "Q", "Q", "J", "J", "J", "J"],
		["5", "10", "8", "2", "6", "7", "2", "4", "9", "2", "6", "10", "10", "5", "4", "8", "4", "8", "6", "9", "8", "5", "9", "3", "4", "9"]
	)
	return [got, want]


func test_mirrored_decks(solution_script):
	var want = {"status": "finished", "cards": 59, "tricks": 4}
	var got = solution_script.simulate_game(
		["2", "A", "3", "A", "3", "K", "4", "K", "2", "Q", "2", "Q", "10", "J", "5", "J", "6", "10", "2", "9", "10", "7", "3", "9", "6", "9"],
		["6", "A", "4", "A", "7", "K", "4", "K", "7", "Q", "7", "Q", "5", "J", "8", "J", "4", "5", "8", "9", "10", "6", "8", "3", "8", "5"]
	)
	return [got, want]


func test_opposite_decks(solution_script):
	var want = {"status": "finished", "cards": 151, "tricks": 21}
	var got = solution_script.simulate_game(
		["4", "A", "9", "A", "4", "K", "9", "K", "6", "Q", "8", "Q", "8", "J", "10", "J", "9", "8", "4", "6", "3", "6", "5", "2", "4", "3"],
		["10", "7", "3", "2", "9", "2", "7", "8", "7", "5", "J", "7", "J", "10", "Q", "10", "Q", "3", "K", "5", "K", "6", "A", "2", "A", "5"]
	)
	return [got, want]


func test_random_decks_1(solution_script):
	var want = {"status": "finished", "cards": 542, "tricks": 76}
	var got = solution_script.simulate_game(
		["K", "10", "9", "8", "J", "8", "6", "9", "7", "A", "K", "5", "4", "4", "J", "5", "J", "4", "3", "5", "8", "6", "7", "7", "4", "9"],
		["6", "3", "K", "A", "Q", "10", "A", "2", "Q", "8", "2", "10", "10", "2", "Q", "3", "K", "9", "7", "A", "3", "Q", "5", "J", "2", "6"]
	)
	return [got, want]


func test_random_decks_2(solution_script):
	var want = {"status": "finished", "cards": 327, "tricks": 42}
	var got = solution_script.simulate_game(
		["8", "A", "4", "8", "5", "Q", "J", "2", "6", "2", "9", "7", "K", "A", "8", "10", "K", "8", "10", "9", "K", "6", "7", "3", "K", "9"],
		["10", "5", "2", "6", "Q", "J", "A", "9", "5", "5", "3", "7", "3", "J", "A", "2", "Q", "3", "J", "Q", "4", "10", "4", "7", "4", "6"]
	)
	return [got, want]


func test_kleber_1999(solution_script):
	var want = {"status": "finished", "cards": 5790, "tricks": 805}
	var got = solution_script.simulate_game(
		["4", "8", "9", "J", "Q", "8", "5", "5", "K", "2", "A", "9", "8", "5", "10", "A", "4", "J", "3", "K", "6", "9", "2", "Q", "K", "7"],
		["10", "J", "3", "2", "4", "10", "4", "7", "5", "3", "6", "6", "7", "A", "J", "Q", "A", "7", "2", "10", "3", "K", "9", "6", "8", "Q"]
	)
	return [got, want]


func test_collins_2006(solution_script):
	var want = {"status": "finished", "cards": 6913, "tricks": 960}
	var got = solution_script.simulate_game(
		["A", "8", "Q", "K", "9", "10", "3", "7", "4", "2", "Q", "3", "2", "10", "9", "K", "A", "8", "7", "7", "4", "5", "J", "9", "2", "10"],
		["4", "J", "A", "K", "8", "5", "6", "6", "A", "6", "5", "Q", "4", "6", "10", "8", "J", "2", "5", "7", "Q", "J", "3", "3", "K", "9"]
	)
	return [got, want]


func test_mann_and_wu_2007(solution_script):
	var want = {"status": "finished", "cards": 7157, "tricks": 1007}
	var got = solution_script.simulate_game(
		["K", "2", "K", "K", "3", "3", "6", "10", "K", "6", "A", "2", "5", "5", "7", "9", "J", "A", "A", "3", "4", "Q", "4", "8", "J", "6"],
		["4", "5", "2", "Q", "7", "9", "9", "Q", "7", "J", "9", "8", "10", "3", "10", "J", "4", "10", "8", "6", "8", "7", "A", "Q", "5", "2"]
	)
	return [got, want]


func test_nessler_2012(solution_script):
	var want = {"status": "finished", "cards": 7207, "tricks": 1015}
	var got = solution_script.simulate_game(
		["10", "3", "6", "7", "Q", "2", "9", "8", "2", "8", "4", "A", "10", "6", "K", "2", "10", "A", "5", "A", "2", "4", "Q", "J", "K", "4"],
		["10", "Q", "4", "6", "J", "9", "3", "J", "9", "3", "3", "Q", "K", "5", "9", "5", "K", "6", "5", "7", "8", "J", "A", "7", "8", "7"]
	)
	return [got, want]


func test_anderson_2013(solution_script):
	var want = {"status": "finished", "cards": 7225, "tricks": 1016}
	var got = solution_script.simulate_game(
		["6", "7", "A", "3", "Q", "3", "5", "J", "3", "2", "J", "7", "4", "5", "Q", "10", "5", "A", "J", "2", "K", "8", "9", "9", "K", "3"],
		["4", "J", "6", "9", "8", "5", "10", "7", "9", "Q", "2", "7", "10", "8", "4", "10", "A", "6", "4", "A", "6", "8", "Q", "K", "K", "2"]
	)
	return [got, want]


func test_rucklidge_2014(solution_script):
	var want = {"status": "finished", "cards": 7959, "tricks": 1122}
	var got = solution_script.simulate_game(
		["8", "J", "2", "9", "4", "4", "5", "8", "Q", "3", "9", "3", "6", "2", "8", "A", "A", "A", "9", "4", "7", "2", "5", "Q", "Q", "3"],
		["K", "7", "10", "6", "3", "J", "A", "7", "6", "5", "5", "8", "10", "9", "10", "4", "2", "7", "K", "Q", "10", "K", "6", "J", "J", "K"]
	)
	return [got, want]


func test_nessler_2021(solution_script):
	var want = {"status": "finished", "cards": 7972, "tricks": 1106}
	var got = solution_script.simulate_game(
		["7", "2", "3", "4", "K", "9", "6", "10", "A", "8", "9", "Q", "7", "A", "4", "8", "J", "J", "A", "4", "3", "2", "5", "6", "6", "J"],
		["3", "10", "8", "9", "8", "K", "K", "2", "5", "5", "7", "6", "4", "3", "5", "7", "A", "9", "J", "K", "2", "Q", "10", "Q", "10", "Q"]
	)
	return [got, want]


func test_nessler_2022(solution_script):
	var want = {"status": "finished", "cards": 8344, "tricks": 1164}
	var got = solution_script.simulate_game(
		["2", "10", "10", "A", "J", "3", "8", "Q", "2", "5", "5", "5", "9", "2", "4", "3", "10", "Q", "A", "K", "Q", "J", "J", "9", "Q", "K"],
		["10", "7", "6", "3", "6", "A", "8", "9", "4", "3", "K", "J", "6", "K", "4", "9", "7", "8", "5", "7", "8", "2", "A", "7", "4", "6"]
	)
	return [got, want]


func test_casella_2024_first_infinite_game_found(solution_script):
	var want = {"status": "loop", "cards": 474, "tricks": 66}
	var got = solution_script.simulate_game(
		["2", "8", "4", "K", "5", "2", "3", "Q", "6", "K", "Q", "A", "J", "3", "5", "9", "8", "3", "A", "A", "J", "4", "4", "J", "7", "5"],
		["7", "7", "8", "6", "10", "10", "6", "10", "7", "2", "Q", "6", "3", "2", "4", "K", "Q", "10", "J", "5", "9", "8", "9", "9", "K", "A"]
	)
	return [got, want]
