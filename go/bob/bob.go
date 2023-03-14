package bob

import (
	"regexp"
	"unicode"
    "strings"
)

func Hey(remark string) string {
	//return regexpHey(strings.TrimSpace(remark))
	//return unicodeHey(strings.TrimSpace(remark))
	return byteHey(strings.TrimSpace(remark))
}

func response(isSilent, isYelling, isQuestion bool) string {
	switch {
	case isSilent:
		return "Fine. Be that way!"
	case isYelling && isQuestion:
		return "Calm down, I know what I'm doing!"
	case isYelling:
		return "Whoa, chill out!"
	case isQuestion:
		return "Sure."
	default:
		return "Whatever."
	}
}

// ---------------------------------------------
var reUpper = regexp.MustCompile(`[[:upper:]]`)
var reLower = regexp.MustCompile(`[[:lower:]]`)

func regexpHey(remark string) string {
	s := len(remark) == 0
	q := strings.HasSuffix(remark, "?")
	y := reUpper.MatchString(remark) && !reLower.MatchString(remark)

	return response(s, y, q)
}

// ---------------------------------------------
func unicodeHey(remark string) string {
	hasUpper := false
	hasLower := false
	isQuestion := false

	for _, char := range remark {
        switch {
		case unicode.IsSpace(char):
		case char == '?' :
			isQuestion = true
		case unicode.IsUpper(char) :
			hasUpper = true
			isQuestion = false
		case unicode.IsLower(char) :
			hasLower = true
			isQuestion = false
        default:
			isQuestion = false
		}
	}

	return response(len(remark) == 0, (hasUpper && !hasLower), isQuestion)
}

// ---------------------------------------------
func byteHey(remark string) string {
	hasUpper := false
	hasLower := false
	isQuestion := false

	for i := 0; i < len(remark); i++ {
		char := remark[i]

		switch char {
		case 9, 10, 11, 12, 13, 32: // whitespace
		case 63:
			isQuestion = true
		default:
            if 65 <= char && char <= 90 {
                hasUpper = true
            } else if 97 <= char && char <= 122 {
                hasLower = true
            }
            isQuestion = false
		}
	}

	return response(len(remark) == 0, (hasUpper && !hasLower), isQuestion)
}

/* benchmarks, no surprise regexp is slower
 *
 * regexp
 * BenchmarkHey-2            201348              5528 ns/op               0 B/op          0 allocs/op
 *
 * iterate over the unicode runes
 * BenchmarkHey-2            365950              2856 ns/op               0 B/op          0 allocs/op
 *
 * iterate over the bytes
 * BenchmarkHey-2           1338582               924.0 ns/op             0 B/op          0 allocs/op
 */
