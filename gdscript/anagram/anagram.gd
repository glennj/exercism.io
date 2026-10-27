func find_anagram(word, candidates):
	var subj_lc = word.to_lower()
	var subj_key = _to_key(word)

	var is_anagram = func(candidate):
		return subj_lc != candidate.to_lower() and subj_key == _to_key(candidate)

	return candidates.filter(is_anagram)



func _to_key(word):
	var letters = word.to_lower().split("")
	letters.sort()
	return letters
