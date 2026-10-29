func test_orange_orange_black_and_red(solution_script):
	var colors = ["orange", "orange", "black", "red"]
	var expected = "33 ohms ±2%"
	return [solution_script.label_decoder(colors), expected]


func test_blue_gray_brown_and_violet(solution_script):
	var colors = ["blue", "grey", "brown", "violet"]
	var expected = "680 ohms ±0.1%"
	return [solution_script.label_decoder(colors), expected]


func test_red_black_red_and_green(solution_script):
	var colors = ["red", "black", "red", "green"]
	var expected = "2 kiloohms ±0.5%"
	return [solution_script.label_decoder(colors), expected]


func test_green_brownm_orange_and_grey(solution_script):
	var colors = ["green", "brown", "orange", "grey"]
	var expected = "51 kiloohms ±0.05%"
	return [solution_script.label_decoder(colors), expected]


func test_only_one_band(solution_script):
	var colors = ["black"]
	var expected = "0 ohms"
	return [solution_script.label_decoder(colors), expected]


func test_orange_orange_yellow_black_and_brown(solution_script):
	var colors = ["orange", "orange", "yellow", "black", "brown"]
	var expected = "334 ohms ±1%"
	return [solution_script.label_decoder(colors), expected]


func test_red_green_yellow_yellow_and_brown(solution_script):
	var colors = ["red", "green", "yellow", "yellow", "brown"]
	var expected = "2.54 megaohms ±1%"
	return [solution_script.label_decoder(colors), expected]


func test_blue_gray_white_brown_and_brown(solution_script):
	var colors = ["blue", "grey", "white", "brown", "brown"]
	var expected = "6.89 kiloohms ±1%"
	return [solution_script.label_decoder(colors), expected]


func test_violet_orange_red_and_gray(solution_script):
	var colors = ["violet", "orange", "red", "grey"]
	var expected = "7.3 kiloohms ±0.05%"
	return [solution_script.label_decoder(colors), expected]


func test_brown_red_orange_green_and_blue(solution_script):
	var colors = ["brown", "red", "orange", "green", "blue"]
	var expected = "12.3 megaohms ±0.25%"
	return [solution_script.label_decoder(colors), expected]


func test_brown_black_brown_yellow_and_violet(solution_script):
	var colors = ["brown", "black", "brown", "yellow", "violet"]
	var expected = "1.01 megaohms ±0.1%"
	return [solution_script.label_decoder(colors), expected]


func test_brown_black_red_and_red(solution_script):
	var colors = ["brown", "black", "red", "red"]
	var expected = "1 kiloohms ±2%"
	return [solution_script.label_decoder(colors), expected]
