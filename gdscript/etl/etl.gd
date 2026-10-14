func transform(legacy: Dictionary) -> Dictionary:
	var new = {}

	for num in legacy.keys():
		var n = int(num)
		for letter in legacy.get(num):
			new.set(letter.to_lower(), n)

	return new

