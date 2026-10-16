func test_orange_and_orange_and_black(solution_script):
	var colors = ["orange", "orange", "black"]
	var expected = "33 ohms"
	return [solution_script.label(colors), expected]


func test_blue_and_grey_and_brown(solution_script):
	var colors = ["blue", "grey", "brown"]
	var expected = "680 ohms"
	return [solution_script.label(colors), expected]


func test_red_and_black_and_red(solution_script):
	var colors = ["red", "black", "red"]
	var expected = "2 kiloohms"
	return [solution_script.label(colors), expected]


func test_green_and_brown_and_orange(solution_script):
	var colors = ["green", "brown", "orange"]
	var expected = "51 kiloohms"
	return [solution_script.label(colors), expected]


func test_yellow_and_violet_and_yellow(solution_script):
	var colors = ["yellow", "violet", "yellow"]
	var expected = "470 kiloohms"
	return [solution_script.label(colors), expected]


func test_blue_and_violet_and_blue(solution_script):
	var colors = ["blue", "violet", "blue"]
	var expected = "67 megaohms"
	return [solution_script.label(colors), expected]


func test_minimum_possible_value(solution_script):
	var colors = ["black", "black", "black"]
	var expected = "0 ohms"
	return [solution_script.label(colors), expected]


func test_maximum_possible_value(solution_script):
	var colors = ["white", "white", "white"]
	var expected = "99 gigaohms"
	return [solution_script.label(colors), expected]


func test_first_two_colors_make_an_invalid_octal_number(solution_script):
	var colors = ["black", "grey", "black"]
	var expected = "8 ohms"
	return [solution_script.label(colors), expected]


func test_ignore_extra_colors(solution_script):
	var colors = ["blue", "green", "yellow", "orange"]
	var expected = "650 kiloohms"
	return [solution_script.label(colors), expected]
