func distance(strand1: String, strand2: String) -> int:
	var dist = 0

	if strand1.length() != strand2.length():
		return Error.ERR_INVALID_PARAMETER

	for i in range(strand1.length()):
		if strand1[i] != strand2[i]:
			dist += 1

	return dist
