@export var card_num: String

var NONDIGIT = RegEx.create_from_string("\\D")	# match a non-digit

const DOUBLED = [
	[0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
	[0, 2, 4, 6, 8, 1, 3, 5, 7, 9],
]


func valid():
	var clean = card_num.remove_char(32)	# remove spaces

	if clean == "0" or NONDIGIT.search(clean):
		return false

	var sum = 0
	var double = 0

	for c in clean.reverse():
		sum += DOUBLED[double][int(c)]
		double = 1 if double == 0 else 0

	return sum % 10 == 0
