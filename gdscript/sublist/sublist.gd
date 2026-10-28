func sublist(list_one: Array, list_two: Array) -> String:
	var len1 = len(list_one)
	var len2 = len(list_two)

	if len1 == len2 and list_one == list_two: return "equal"
	if len1 < len2 and contains(list_two, list_one): return "sublist"
	if len1 > len2 and contains(list_one, list_two): return "superlist"
	return "unequal"


func contains(larger, smaller):
	var span = len(smaller)
	for i in range(len(larger) - span + 1):
		if smaller == larger.slice(i, i + span):
			return true
	return false
