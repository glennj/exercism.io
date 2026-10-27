func test_empty_strand(solution_script):
	var got = solution_script.nucleotide_counts("")
	var want = {"A": 0, "C": 0, "G": 0, "T": 0}
	return [got, want]


func test_can_count_one_nucleotide_in_single_character_input(solution_script):
	var got = solution_script.nucleotide_counts("G")
	var want = {"A": 0, "C": 0, "G": 1, "T": 0}
	return [got, want]


func test_strand_with_repeated_nucleotide(solution_script):
	var got = solution_script.nucleotide_counts("GGGGGGG")
	var want = {"A": 0, "C": 0, "G": 7, "T": 0}
	return [got, want]


func test_strand_with_multiple_nucleotides(solution_script):
	var got = solution_script.nucleotide_counts("AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC")
	var want = {"A": 20, "C": 12, "G": 17, "T": 21}
	return [got, want]


func test_strand_with_invalid_nucleotides(solution_script):
	var got = solution_script.nucleotide_counts("AGXXACT")
	var want = null
	return [got, want]
