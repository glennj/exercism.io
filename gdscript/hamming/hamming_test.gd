func test_empty_strands(solution_script):
	var strand1 = ""
	var strand2 = ""
	var got = solution_script.distance(strand1, strand2)
	var want = 0
	return [got, want]


func test_single_letter_identical_strands(solution_script):
	var strand1 = "A"
	var strand2 = "A"
	var got = solution_script.distance(strand1, strand2)
	var want = 0
	return [got, want]


func test_single_letter_different_strands(solution_script):
	var strand1 = "G"
	var strand2 = "T"
	var got = solution_script.distance(strand1, strand2)
	var want = 1
	return [got, want]


func test_long_identical_strands(solution_script):
	var strand1 = "GGACTGAAATCTG"
	var strand2 = "GGACTGAAATCTG"
	var got = solution_script.distance(strand1, strand2)
	var want = 0
	return [got, want]


func test_long_different_strands(solution_script):
	var strand1 = "GGACGGATTCTG"
	var strand2 = "AGGACGGATTCT"
	var got = solution_script.distance(strand1, strand2)
	var want = 9
	return [got, want]


func test_disallow_first_strand_longer(solution_script):
	var strand1 = "AATG"
	var strand2 = "AAA"
	var got = solution_script.distance(strand1, strand2)
	var want = ERR_INVALID_PARAMETER
	return [got, want]


func test_disallow_second_strand_longer(solution_script):
	var strand1 = "ATA"
	var strand2 = "AGTG"
	var got = solution_script.distance(strand1, strand2)
	var want = ERR_INVALID_PARAMETER
	return [got, want]


func test_disallow_empty_first_strand(solution_script):
	var strand1 = ""
	var strand2 = "G"
	var got = solution_script.distance(strand1, strand2)
	var want = ERR_INVALID_PARAMETER
	return [got, want]


func test_disallow_empty_second_strand(solution_script):
	var strand1 = "G"
	var strand2 = ""
	var got = solution_script.distance(strand1, strand2)
	var want = ERR_INVALID_PARAMETER
	return [got, want]
