func test_encode_empty_string(solution_script):
	var got = solution_script.encode("")
	var want = ""
	return [got, want]


func test_encode_single_characters_only_are_encoded_without_count(solution_script):
	var got = solution_script.encode("XYZ")
	var want = "XYZ"
	return [got, want]


func test_encode_string_with_no_single_characters(solution_script):
	var got = solution_script.encode("AABBBCCCC")
	var want = "2A3B4C"
	return [got, want]


func test_encode_single_characters_mixed_with_repeated_characters(solution_script):
	var got = solution_script.encode("WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB")
	var want = "12WB12W3B24WB"
	return [got, want]


func test_encode_multiple_whitespace_mixed_in_string(solution_script):
	var got = solution_script.encode("  hsqq qww  ")
	var want = "2 hs2q q2w2 "
	return [got, want]


func test_encode_lowercase_characters(solution_script):
	var got = solution_script.encode("aabbbcccc")
	var want = "2a3b4c"
	return [got, want]


func test_decode_empty_string(solution_script):
	var got = solution_script.decode("")
	var want = ""
	return [got, want]


func test_decode_single_characters_only(solution_script):
	var got = solution_script.decode("XYZ")
	var want = "XYZ"
	return [got, want]


func test_decode_string_with_no_single_characters(solution_script):
	var got = solution_script.decode("2A3B4C")
	var want = "AABBBCCCC"
	return [got, want]


func test_decode_single_characters_with_repeated_characters(solution_script):
	var got = solution_script.decode("12WB12W3B24WB")
	var want = "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB"
	return [got, want]


func test_decode_multiple_whitespace_mixed_in_string(solution_script):
	var got = solution_script.decode("2 hs2q q2w2 ")
	var want = "  hsqq qww  "
	return [got, want]


func test_decode_lowercase_string(solution_script):
	var got = solution_script.decode("2a3b4c")
	var want = "aabbbcccc"
	return [got, want]
