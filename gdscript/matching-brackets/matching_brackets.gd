func is_paired(data: String) -> bool:
	var stack = []

	for c in data:
		match c:
			"{", "[", "(":
				stack.push_front(c)

			"}", "]", ")":
				if stack.is_empty() or not is_match(stack.pop_front(), c):
					return false

	return stack.is_empty()


func is_match(opener, closer):
	return ["{}", "[]", "()"].has(opener + closer)

