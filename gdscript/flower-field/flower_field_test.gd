func test_no_rows(solution_script):
	var got = solution_script.annotate([
	])
	var want = [
	]
	return [got, want]


func test_no_columns(solution_script):
	var got = solution_script.annotate([
		"",
	])
	var want = [
		"",
	]
	return [got, want]


func test_no_flowers(solution_script):
	var got = solution_script.annotate([
		"   ",
		"   ",
		"   ",
	])
	var want = [
		"   ",
		"   ",
		"   ",
	]
	return [got, want]


func test_garden_full_of_flowers(solution_script):
	var got = solution_script.annotate([
		"***",
		"***",
		"***",
	])
	var want = [
		"***",
		"***",
		"***",
	]
	return [got, want]


func test_flower_surrounded_by_spaces(solution_script):
	var got = solution_script.annotate([
		"   ",
		" * ",
		"   ",
	])
	var want = [
		"111",
		"1*1",
		"111",
	]
	return [got, want]


func test_space_surrounded_by_flowers(solution_script):
	var got = solution_script.annotate([
		"***",
		"* *",
		"***",
	])
	var want = [
		"***",
		"*8*",
		"***",
	]
	return [got, want]


func test_horizontal_line(solution_script):
	var got = solution_script.annotate([
		" * * ",
	])
	var want = [
		"1*2*1",
	]
	return [got, want]


func test_horizontal_line_flowers_at_edges(solution_script):
	var got = solution_script.annotate([
		"*   *",
	])
	var want = [
		"*1 1*",
	]
	return [got, want]


func test_vertical_line(solution_script):
	var got = solution_script.annotate([
		" ",
		"*",
		" ",
		"*",
		" ",
	])
	var want = [
		"1",
		"*",
		"2",
		"*",
		"1",
	]
	return [got, want]


func test_vertical_line_flowers_at_edges(solution_script):
	var got = solution_script.annotate([
		"*",
		" ",
		" ",
		" ",
		"*",
	])
	var want = [
		"*",
		"1",
		" ",
		"1",
		"*",
	]
	return [got, want]


func test_cross(solution_script):
	var got = solution_script.annotate([
		"  *  ",
		"  *  ",
		"*****",
		"  *  ",
		"  *  ",
	])
	var want = [
		" 2*2 ",
		"25*52",
		"*****",
		"25*52",
		" 2*2 ",
	]
	return [got, want]


func test_large_garden(solution_script):
	var got = solution_script.annotate([
		" *  * ",
		"  *   ",
		"    * ",
		"   * *",
		" *  * ",
		"      ",
	])
	var want = [
		"1*22*1",
		"12*322",
		" 123*2",
		"112*4*",
		"1*22*2",
		"111111",
	]
	return [got, want]


func test_multiple_adjacent_flowers(solution_script):
	var got = solution_script.annotate([
		" ** ",
	])
	var want = [
		"1**1",
	]
	return [got, want]
