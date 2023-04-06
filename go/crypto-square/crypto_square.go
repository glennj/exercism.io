package cryptosquare

import (
	"math"
	"strings"
	"unicode"
)

func Encode(pt string) string {
	mapper := func(r rune) rune {
		switch {
		case unicode.IsLower(r) || unicode.IsDigit(r):
			return r
		case unicode.IsUpper(r):
			return unicode.ToLower(r)
		default:
			return -1
		}
	}

	normalized := strings.Map(mapper, pt)
	if len(normalized) == 0 {
		return ""
	}

	segmentLength := int(math.Ceil(math.Sqrt(float64(len(normalized)))))
	segments := make([]string, 0, (len(normalized)/segmentLength)+1)

	for len(normalized) >= segmentLength {
		segments = append(segments, normalized[:segmentLength])
		normalized = normalized[segmentLength:]
	}
	if len(normalized) > 0 {
		normalized += strings.Repeat(" ", segmentLength)
		segments = append(segments, normalized[:segmentLength])
	}

	transposed := strings.Builder{}
	for i := 0; i < segmentLength; i++ {
		for _, seg := range segments {
			transposed.WriteByte(seg[i])
		}
		transposed.WriteByte(' ')
	}
	return strings.TrimSuffix(transposed.String(), " ")

}

// bench
// BenchmarkEncode-2         146997              7869 ns/op            2680 B/op         92 allocs/op
