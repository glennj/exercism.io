func count_words(sentence: String):
	var counts = {}
	var word_re = RegEx.create_from_string("\\b\\w['\\w]*")

	for re_match in word_re.search_all(sentence):
		var word = re_match.get_string().to_lower().trim_suffix("'")
		counts[word] = counts.get(word, 0) + 1

	return counts
