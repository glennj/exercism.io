func find(search_list, value):
	var left = 0
	var right = search_list.size() - 1

	while left <= right:
		var mid = (left + right) / 2

		if value < search_list[mid]:
			right = mid - 1
		elif value > search_list[mid]:
			left = mid + 1
		else:
			return mid

	return null
