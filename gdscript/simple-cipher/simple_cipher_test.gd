func test_random_key_cipher_can_encode(solution_script):
	var cipher = solution_script.Cipher.new()
	var plaintext = "aaaaaaaaaa"
	var got = cipher.encode(plaintext)
	var want = cipher.key.substr(0, len(plaintext))
	return [got, want]


func test_random_key_cipher_can_decode(solution_script):
	var cipher = solution_script.Cipher.new()
	var want = "aaaaaaaaaa"
	var got = cipher.decode(cipher.key.substr(0, len(want)))
	return [got, want]


func test_key_is_made_only_of_lowercase_letters(solution_script):
	var cipher = solution_script.Cipher.new()
	var is_lowercase = true
	var letters = "abcdefghijklmnopqrstuvwxyz"
	for char in cipher.key:
		if char not in letters:
			is_lowercase = false
	return [is_lowercase, true]


func test_can_encode(solution_script):
	var got = solution_script.Cipher.new("abcdefghij").encode("aaaaaaaaaa")
	var want = "abcdefghij"
	return [got, want]


func test_can_decode(solution_script):
	var got = solution_script.Cipher.new("abcdefghij").decode("abcdefghij")
	var want = "aaaaaaaaaa"
	return [got, want]


func test_can_double_shift_encode(solution_script):
	var got = solution_script.Cipher.new("iamapandabear").encode("iamapandabear")
	var want = "qayaeaagaciai"
	return [got, want]


func test_can_wrap_on_encode(solution_script):
	var got = solution_script.Cipher.new("abcdefghij").encode("zzzzzzzzzz")
	var want = "zabcdefghi"
	return [got, want]


func test_can_wrap_on_decode(solution_script):
	var got = solution_script.Cipher.new("abcdefghij").decode("zabcdefghi")
	var want = "zzzzzzzzzz"
	return [got, want]


func test_can_encode_messages_longer_than_the_key(solution_script):
	var got = solution_script.Cipher.new("abc").encode("iamapandabear")
	var want = "iboaqcnecbfcr"
	return [got, want]


func test_can_decode_messages_longer_than_the_key(solution_script):
	var got = solution_script.Cipher.new("abc").decode("iboaqcnecbfcr")
	var want = "iamapandabear"
	return [got, want]
