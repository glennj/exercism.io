func is_isogram(string: String):
	var s = string.to_lower().remove_chars(" -")
	var seen = {}

	for c in s:
		if seen.has(c):
			return false

		seen.set(c, true)

	return true
