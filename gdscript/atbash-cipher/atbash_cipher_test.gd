func test_encode_yes(solution_script):
	var got = solution_script.encode("yes")
	var want = "bvh"
	return [got, want]


func test_encode_no(solution_script):
	var got = solution_script.encode("no")
	var want = "ml"
	return [got, want]


func test_encode_omg(solution_script):
	var got = solution_script.encode("OMG")
	var want = "lnt"
	return [got, want]


func test_encode_spaces(solution_script):
	var got = solution_script.encode("O M G")
	var want = "lnt"
	return [got, want]


func test_encode_mindblowingly(solution_script):
	var got = solution_script.encode("mindblowingly")
	var want = "nrmwy oldrm tob"
	return [got, want]


func test_encode_numbers(solution_script):
	var got = solution_script.encode("Testing,1 2 3, testing.")
	var want = "gvhgr mt123 gvhgr mt"
	return [got, want]


func test_encode_deep_thought(solution_script):
	var got = solution_script.encode("Truth is fiction.")
	var want = "gifgs rhurx grlm"
	return [got, want]


func test_encode_all_the_letters(solution_script):
	var got = solution_script.encode("The quick brown fox jumps over the lazy dog.")
	var want = "gsvjf rxpyi ldmul cqfnk hlevi gsvoz abwlt"
	return [got, want]


func test_decode_exercism(solution_script):
	var got = solution_script.decode("vcvix rhn")
	var want = "exercism"
	return [got, want]


func test_decode_a_sentence(solution_script):
	var got = solution_script.decode("zmlyh gzxov rhlug vmzhg vkkrm thglm v")
	var want = "anobstacleisoftenasteppingstone"
	return [got, want]


func test_decode_numbers(solution_script):
	var got = solution_script.decode("gvhgr mt123 gvhgr mt")
	var want = "testing123testing"
	return [got, want]


func test_decode_all_the_letters(solution_script):
	var got = solution_script.decode("gsvjf rxpyi ldmul cqfnk hlevi gsvoz abwlt")
	var want = "thequickbrownfoxjumpsoverthelazydog"
	return [got, want]


func test_decode_with_too_many_spaces(solution_script):
	var got = solution_script.decode("vc vix    r hn")
	var want = "exercism"
	return [got, want]


func test_decode_with_no_spaces(solution_script):
	var got = solution_script.decode("zmlyhgzxovrhlugvmzhgvkkrmthglmv")
	var want = "anobstacleisoftenasteppingstone"
	return [got, want]
