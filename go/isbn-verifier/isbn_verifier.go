package isbn

func IsValidISBN(isbn string) bool {
	sum := 0
	idx := 10

	for _, c := range isbn {
        switch {
        case '0' <= c && c <= '9':
			sum += idx * int(c-'0')
			idx -= 1
        case c == 'X' && idx == 1:
			sum += 10
			idx -= 1
        case c == '-':
        default:
			return false
		}
	}

	return idx == 0 && sum%11 == 0
}

/* bench
 * BenchmarkIsValidISBN-2           6406516               183.3 ns/op             0 B/op          0 allocs/op
 */
