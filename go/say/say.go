package say

func Say(n int64) (string, bool) {
	switch {
	case n < 0 || n >= 1_000_000_000_000:
		return "", false
	case n < 100:
		return small(n), true
	case n < 1_000:
		return compound(n, 100, "hundred"), true
	case n < 1_000_000:
		return compound(n, 1_000, "thousand"), true
	case n < 1_000_000_000:
		return compound(n, 1_000_000, "million"), true
	default:
		return compound(n, 1_000_000_000, "billion"), true
	}
}

func divmod(n int64, divisor int64) (int64, int64) {
	return (n / divisor), (n % divisor)
}

func compound(n int64, base int64, unit string) string {
	quo, rem := divmod(n, base)
	result, _ := Say(quo)
	result += " " + unit
	if rem != 0 {
		rest, _ := Say(rem)
		result += " " + rest
	}
	return result
}

func small(n int64) string {
	switch {
	case n ==  0: return "zero"
	case n ==  1: return "one"
	case n ==  2: return "two"
	case n ==  3: return "three"
	case n ==  4: return "four"
	case n ==  5: return "five"
	case n ==  6: return "six"
	case n ==  7: return "seven"
	case n ==  8: return "eight"
	case n ==  9: return "nine"
	case n == 10: return "ten"
	case n == 11: return "eleven"
	case n == 12: return "twelve"
	case n == 13: return "thirteen"
	case n == 14: return "fourteen"
	case n == 15: return "fifteen"
	case n == 16: return "sixteen"
	case n == 17: return "seventeen"
	case n == 18: return "eighteen"
	case n == 19: return "nineteen"
	case n == 20: return "twenty"
	case n == 30: return "thirty"
	case n == 40: return "forty"
	case n == 50: return "fifty"
	case n == 60: return "sixty"
	case n == 70: return "seventy"
	case n == 80: return "eighty"
	case n == 90: return "ninety"
	case n <= 99:
		tens, ones := divmod(n, 10)
		result := small(tens * 10)
		if ones > 0 {
			result += "-" + small(ones)
		}
		return result
	default:
		panic("don't call small with n >= 100")
	}
}

/* bench
 * BenchmarkSay-2            517016              2282 ns/op            1192 B/op         43 allocs/op
*/
