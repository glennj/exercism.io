package lsproduct

import "errors"

var (
	ErrSpanTooBig   = errors.New("span must be smaller then string length")
	ErrSpanTooSmall = errors.New("span must not be negative")
	ErrNonDigit     = errors.New("digits input must only contain digits")
)

func LargestSeriesProduct(digits string, span int) (max int64, err error) {
    max, err = iterate_over_substrings(digits, span)
    //max, err = iterate_over_ints(digits, span)
    return
}

// ------------------------------------------------------------
func iterate_over_substrings(digits string, span int) (max int64, err error) {
    err = validate(digits, span)
    if err != nil {
        return 0, err
    }

    if span == 0 {
        return int64(1), nil
    }

	for i := len(digits) - span; i >= 0; i-- {
		p := product(digits[i : i+span])
		if p > max {
			max = p
		}
	}

	return max, nil
}

func validate(digits string, span int) error {
	if span < 0 {
		return ErrSpanTooSmall
	}
	if span > len(digits) {
		return ErrSpanTooBig
	}
	for _, c := range digits {
		if c < '0' || c > '9' {
			return ErrNonDigit
		}
	}
    return nil
}

func product(digits string) (product int64) {
	product = 1
	for _, d := range digits {
		product *= int64(d - '0')
	}
	return
}

// ------------------------------------------------------------
func iterate_over_ints(digitString string, span int) (max int64, err error) {
	if span < 0 {
		return 0, ErrSpanTooSmall
	}
	if span > len(digitString) {
		return 0, ErrSpanTooBig
	}

    digits := make([]int64, len(digitString))
    for i, d := range digitString {
        n := int64(d - '0')
        if n < 0 || n > 9 {
            return 0, ErrNonDigit
        }
        digits[i] = n
    }

    if span == 0 {
        return 1, nil
    }

    for i := len(digits) - span; i >= 0; i-- {
        product := int64(1)
        for j := i; j < i+span; j++ {
            product *= digits[j]
        }
        if product > max {
            max = product
        }
    }
    return max, nil
}

/* bench
 *
 * iterating over substrings
 * BenchmarkLargestSeriesProduct-2          2346115               503.3 ns/op             0 B/op          0 allocs/op
 *
 * iterating over a slice of ints is slower: have to allocate the slice
 * BenchmarkLargestSeriesProduct-2          1439634               814.7 ns/op           984 B/op         11 allocs/op
 */
