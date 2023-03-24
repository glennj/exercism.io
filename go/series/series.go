package series

func All(n int, s string) []string {
	series := make([]string, 0, len(s)-n+1)
	for i := 0; i+n <= len(s); i++ {
		series = append(series, s[i:i+n])
	}
	return series
}

// go test -tags asktoomuch
func UnsafeFirst(n int, s string) string {
	return s[:n]
}

// go test -tags first
func First(n int, s string) (first string, ok bool) {
	if n <= len(s) {
		ok = true
		first = s[:n]
	}
	return
}
