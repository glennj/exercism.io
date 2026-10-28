const FOOD = ["fly", "spider", "bird", "cat", "dog", "goat", "cow", "horse"]

const WRIGGLED = "wriggled and jiggled and tickled inside her"

const CATCHPHRASE = {
	fly = "I don't know why she swallowed the fly. Perhaps she'll die.",
	spider = "It %s." % WRIGGLED,
	bird = "How absurd to swallow a bird!",
	cat = "Imagine that, to swallow a cat!",
	dog = "What a hog, to swallow a dog!",
	goat = "Just opened her throat and swallowed a goat!",
	cow = "I don't know how she swallowed a cow!",
	horse = "She's dead, of course!",
}


func recite(start_verse: int, end_verse: int) -> String:
	var verses = range(start_verse - 1, end_verse).map(verse)
	return "\n\n".join(verses)


func verse(n):
	var v = ["I know an old lady who swallowed a %s." % FOOD[n]]
	v.append(CATCHPHRASE[FOOD[n]])

	if FOOD[n] in ["fly", "horse"]:
		return "\n".join(v)

	for i in range(n, 0, -1):
		v.append("She swallowed the %s to catch the %s." % [FOOD[i], previous(i)])

	v.append(CATCHPHRASE.fly)
	return "\n".join(v)


func previous(n):
	return FOOD[n-1] + ("" if FOOD[n-1] != 'spider' else " that " + WRIGGLED)

