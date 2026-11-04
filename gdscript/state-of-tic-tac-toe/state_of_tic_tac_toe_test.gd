func test_finished_game_where_x_won_via_left_column_victory(solution_script):
	var got = solution_script.gamestate([
		"XOO",
		"X  ",
		"X  ",
	])
	var want = "win"
	return [got, want]


func test_finished_game_where_x_won_via_middle_column_victory(solution_script):
	var got = solution_script.gamestate([
		"OXO",
		" X ",
		" X ",
	])
	var want = "win"
	return [got, want]


func test_finished_game_where_x_won_via_right_column_victory(solution_script):
	var got = solution_script.gamestate([
		"OOX",
		"  X",
		"  X",
	])
	var want = "win"
	return [got, want]


func test_finished_game_where_o_won_via_left_column_victory(solution_script):
	var got = solution_script.gamestate([
		"OXX",
		"OX ",
		"O  ",
	])
	var want = "win"
	return [got, want]


func test_finished_game_where_o_won_via_middle_column_victory(solution_script):
	var got = solution_script.gamestate([
		"XOX",
		" OX",
		" O ",
	])
	var want = "win"
	return [got, want]


func test_finished_game_where_o_won_via_right_column_victory(solution_script):
	var got = solution_script.gamestate([
		"XXO",
		" XO",
		"  O",
	])
	var want = "win"
	return [got, want]


func test_finished_game_where_x_won_via_top_row_victory(solution_script):
	var got = solution_script.gamestate([
		"XXX",
		"XOO",
		"O  ",
	])
	var want = "win"
	return [got, want]


func test_finished_game_where_x_won_via_middle_row_victory(solution_script):
	var got = solution_script.gamestate([
		"O  ",
		"XXX",
		" O ",
	])
	var want = "win"
	return [got, want]


func test_finished_game_where_x_won_via_bottom_row_victory(solution_script):
	var got = solution_script.gamestate([
		" OO",
		"O X",
		"XXX",
	])
	var want = "win"
	return [got, want]


func test_finished_game_where_o_won_via_top_row_victory(solution_script):
	var got = solution_script.gamestate([
		"OOO",
		"XXO",
		"XX ",
	])
	var want = "win"
	return [got, want]


func test_finished_game_where_o_won_via_middle_row_victory(solution_script):
	var got = solution_script.gamestate([
		"XX ",
		"OOO",
		"X  ",
	])
	var want = "win"
	return [got, want]


func test_finished_game_where_o_won_via_bottom_row_victory(solution_script):
	var got = solution_script.gamestate([
		"XOX",
		" XX",
		"OOO",
	])
	var want = "win"
	return [got, want]


func test_finished_game_where_x_won_via_falling_diagonal_victory(solution_script):
	var got = solution_script.gamestate([
		"XOO",
		" X ",
		"  X",
	])
	var want = "win"
	return [got, want]


func test_finished_game_where_x_won_via_rising_diagonal_victory(solution_script):
	var got = solution_script.gamestate([
		"O X",
		"OX ",
		"X  ",
	])
	var want = "win"
	return [got, want]


func test_finished_game_where_o_won_via_falling_diagonal_victory(solution_script):
	var got = solution_script.gamestate([
		"OXX",
		"OOX",
		"X O",
	])
	var want = "win"
	return [got, want]


func test_finished_game_where_o_won_via_rising_diagonal_victory(solution_script):
	var got = solution_script.gamestate([
		"  O",
		" OX",
		"OXX",
	])
	var want = "win"
	return [got, want]


func test_finished_game_where_x_won_via_a_row_and_a_column_victory(solution_script):
	var got = solution_script.gamestate([
		"XXX",
		"XOO",
		"XOO",
	])
	var want = "win"
	return [got, want]


func test_finished_game_where_x_won_via_two_diagonal_victories(solution_script):
	var got = solution_script.gamestate([
		"XOX",
		"OXO",
		"XOX",
	])
	var want = "win"
	return [got, want]


func test_draw(solution_script):
	var got = solution_script.gamestate([
		"XOX",
		"XXO",
		"OXO",
	])
	var want = "draw"
	return [got, want]


func test_another_draw(solution_script):
	var got = solution_script.gamestate([
		"XXO",
		"OXX",
		"XOO",
	])
	var want = "draw"
	return [got, want]


func test_ongoing_game_one_move_in(solution_script):
	var got = solution_script.gamestate([
		"   ",
		"X  ",
		"   ",
	])
	var want = "ongoing"
	return [got, want]


func test_ongoing_game_two_moves_in(solution_script):
	var got = solution_script.gamestate([
		"O  ",
		" X ",
		"   ",
	])
	var want = "ongoing"
	return [got, want]


func test_ongoing_game_five_moves_in(solution_script):
	var got = solution_script.gamestate([
		"X  ",
		" XO",
		"OX ",
	])
	var want = "ongoing"
	return [got, want]


func test_invalid_board_x_went_twice(solution_script):
	var got = solution_script.gamestate([
		"XX ",
		"   ",
		"   ",
	])
	var want = null
	return [got, want]


func test_invalid_board_o_started(solution_script):
	var got = solution_script.gamestate([
		"OOX",
		"   ",
		"   ",
	])
	var want = null
	return [got, want]


func test_invalid_board_x_won_and_o_kept_playing(solution_script):
	var got = solution_script.gamestate([
		"XXX",
		"OOO",
		"   ",
	])
	var want = null
	return [got, want]


func test_invalid_board_players_kept_playing_after_a_win(solution_script):
	var got = solution_script.gamestate([
		"XXX",
		"OOO",
		"XOX",
	])
	var want = null
	return [got, want]


func test_invalid_board_o_kept_playing_after_x_wins(solution_script):
	var got = solution_script.gamestate([
		"OO ",
		"XXX",
		" O ",
	])
	var want = null
	return [got, want]


func test_invalid_board_x_kept_playing_after_o_wins(solution_script):
	var got = solution_script.gamestate([
		"XX ",
		"OOO",
		" XX",
	])
	var want = null
	return [got, want]
