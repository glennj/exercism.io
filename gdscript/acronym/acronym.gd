const ALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

enum State {SEEKING_LETTER, SEEKING_NON_LETTER}


func is_letter(char): return ALPHABET.find(char) != -1

func is_word_char(char): return is_letter(char) or char == "'"


func abbreviate(words):
	var acronym = ""
	var state = State.SEEKING_LETTER

	for c in words.to_upper():
		match state:
			State.SEEKING_LETTER:
				if is_letter(c):
					acronym += c
					state = State.SEEKING_NON_LETTER

			State.SEEKING_NON_LETTER:
				if not is_word_char(c):
					state = State.SEEKING_LETTER

	return acronym
