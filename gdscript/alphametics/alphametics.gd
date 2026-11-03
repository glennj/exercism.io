func solve(puzzle):
	var words = RegEx.create_from_string("[A-Z]+") \
			.search_all(puzzle) \
			.map(func (m): return m.get_string())

	var first_letters = words.map(func (w): return w[0])
	var last_letters = words.map(func (w): return w[-1])
	var uniq_letters = sort_uniq_letters(puzzle, last_letters)

	var helper = func(recur, mapping, idx):
		if idx >= uniq_letters.size():
			# we've mapped all letters: do we have a solution?
			return mapping if solves_puzzle(words, mapping) else null

		var letter = uniq_letters[idx]

		# the first letter of an addend cannot be zero
		var start = 1 if first_letters.has(letter) else 0

		for digit in range(start, 10):
			if mapping.values().has(digit): continue

			mapping[letter] = digit

			# test the constraint that the last digits add up correctly
			var check = last_digits_valid(mapping, last_letters)
			if check == true or check == null:
				# recurse with current mappiong to test next letter
				var result = recur.call(recur, mapping, idx + 1)
				if result:
					return result

			# no solution for this mapping: try the next digit
			mapping.erase(letter)

		# no solution for this mapping
		return null

	var result = helper.call(helper, {}, 0)
	return result


func solves_puzzle(words, mapping):
	var word_to_num = func(word):
		var n = Array(word.split()) \
				.reduce(func(sum, c): return sum * 10 + mapping[c], 0)
		return n

	var nums = words.map(word_to_num)
	var sum = nums.reduce(func(a, n): return a + n)
	var last = nums[-1]
	return sum - last == last


# The sum of the last digit of each addend must correspond with
# the last digit of the total.
# The function returns one of 3 values:
#   null,  if not all the letters have been mapped
#   true,  if the last digit sum is valid
#   false, otherwise
func last_digits_valid(mapping, last_letters):
	var sum = 0
	if not last_letters.all(func (c): return mapping.has(c)):
		return null
	for i in range(last_letters.size() - 1):
		sum += mapping[last_letters[i]]
	return sum % 10 == mapping[last_letters[-1]]


# We prioritize solving last letters first (that's a main constraint).
# Then we order by frequency, most-used first.
func sort_uniq_letters(puzzle, last_letters):
	var all = {}
	for c in puzzle.remove_chars(" +="):
		all[c] = all.get(c, 0) + 1
	
	var last_count = {}
	for c in last_letters:
		last_count[c] = last_count.get(c, 0) + 1

	var rest = {}
	for c in all:
		if not c in last_count:
			rest[c] = all[c]

	var order_keys = func (dict):
		var keys = dict.keys()
		keys.sort_custom(func (a, b): return dict[a] > dict[b])
		return keys

	var ordered = order_keys.call(last_count)
	ordered.append_array(order_keys.call(rest))
	return ordered
