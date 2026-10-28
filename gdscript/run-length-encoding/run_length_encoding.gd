var decode_re = RegEx.create_from_string("(?<n>\\d*)(?<c>.)")

func decode(string):
	var decoded = ""
	for m in decode_re.search_all(string):
		var n = m.get_string("n")
		var count = 1 if n == "" else int(n)
		decoded += m.get_string("c").repeat(count)
	return decoded


var encode_re = RegEx.create_from_string("(?<c>.)(?<rest>\\1*)")

func encode(string):
	var encoded = ""
	for m in encode_re.search_all(string):
		var r = m.get_string("rest")
		var count = "" if r == "" else str(1 + len(r))
		encoded += count + m.get_string("c")
	return encoded
