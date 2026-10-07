func response(message: String) -> String:
	var trimmed = message.strip_edges()
	var is_silent = trimmed.is_empty()
	var is_question = trimmed.ends_with("?")

	# yelling: contains an upper and does not contain a lower
	var yelling = RegEx.create_from_string("^(?=.*[[:upper:]])(?!.*[[:lower:]])")
	var is_yelling = not not yelling.search(trimmed) # coerce RegExMatch to a boolean

	match [is_silent, is_question, is_yelling]:
		[true, _, _]:    return "Fine. Be that way!"
		[_, true, true]: return "Calm down, I know what I'm doing!"
		[_, true, _]:    return "Sure."
		[_, _, true]:    return "Whoa, chill out!"
		_:               return "Whatever."
