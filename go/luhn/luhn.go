package luhn

var digitValue = [][]int{
	{0, 1, 2, 3, 4, 5, 6, 7, 8, 9},
	{0, 2, 4, 6, 8, 1, 3, 5, 7, 9},
}

func Valid(id string) bool {
	sum := 0
	idx := 0

	for i := len(id) - 1; i >= 0; i-- {
		c := int(id[i])
		switch {
		case c == 32: // ignore spaces
		case 48 <= c && c <= 57: // digit
			sum += digitValue[idx % 2][c - 48]
			idx++
		default:
			return false
		}
	}

	return idx >= 2 && sum % 10 == 0
}
