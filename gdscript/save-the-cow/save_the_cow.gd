class SaveTheCow:
	var _word: String
	var _guessed: Array[String]

	var state: String
	var remaining_failures: int
	var error = null

	var masked_word: String:
		get:
			return "".join(Array(_word.split()) \
					.map(func (c): return c if _guessed.has(c) else "_"))
		set(x):
			pass


	func _init(word):
		_word = word
		_guessed = []
		state = "Ongoing"
		remaining_failures = 9


	func guess(letter):
		match state:
			"Lose":
				error = "cannot guess after the game is lost"
			"Win":
				error = "cannot guess after the game is won"
			_:
				if _guessed.has(letter) or not _word.contains(letter):
					remaining_failures -= 1
					if remaining_failures < 0:
						state = "Lose"
						remaining_failures = 0
				else:
					_guessed.append(letter)
					if not masked_word.contains("_"):
						state = "Win"
