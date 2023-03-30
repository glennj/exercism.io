package wordy

import (
	"regexp"
	"strconv"
	"strings"
)

func Answer(question string) (answer int, ok bool) {
	//answer, ok = withRegexp(question)
	answer, ok = withFields(question)
	return
}

// ------------------------------------------------------------
var (
	expr    = regexp.MustCompile(`^(-?\d+) (plus|minus|multiplied by|divided by) (-?\d+)`)
	integer = regexp.MustCompile(`^-?\d+$`)
)

func withRegexp(question string) (int, bool) {
	question = strings.TrimPrefix(question, `What is `)
	question = strings.TrimSuffix(question, `?`)

	var ok bool
	for !integer.MatchString(question) {
		question, ok = process(question)
		if !ok {
			return 0, false
		}
	}

	// should not error, unless there are too many digits
	answer, err := strconv.Atoi(question)
	if err != nil {
		return 0, false
	}

	return answer, true
}

func process(question string) (string, bool) {
	// can't find an arithmetic expression at the start of the string
	submatch := expr.FindStringSubmatch(question)
	if submatch == nil {
		return "", false
	}

	// submatch contains 4 elements
	// 0. prefix string matching the pattern
	// 1. first operand
	// 2. arithmetic operation words
	// 3. second operand

	// Atoi might error if there are too many digits
	a, err := strconv.Atoi(submatch[1])
	if err != nil {
		return "", false
	}
	b, err := strconv.Atoi(submatch[3])
	if err != nil {
		return "", false
	}

	var subresult int
	switch submatch[2] {
	case "plus":
		subresult = a + b
	case "minus":
		subresult = a - b
	case "multiplied by":
		subresult = a * b
	case "divided by":
		subresult = a / b
	default:
		// unknown operation
		return "", false
	}

	question = strconv.Itoa(subresult) + strings.TrimPrefix(question, submatch[0])
	return question, true
}

// ------------------------------------------------------------
// states
const (
	seekingNum int = iota
	seekingOp
)

func add(a, b int) int      { return a + b }
func subtract(a, b int) int { return a - b }
func multiply(a, b int) int { return a * b }
func divide(a, b int) int   { return a / b }

func withFields(question string) (int, bool) {
	question = strings.TrimPrefix(question, `What is `)
	question = strings.TrimSuffix(question, `?`)

	var (
		fields    = strings.Fields(question)
		state     = seekingNum
		operation = add
		result    = 0
		word      string
	)

	for len(fields) > 0 {
		word, fields = fields[0], fields[1:]

		switch state {
		case seekingNum:
			n, err := strconv.Atoi(word)
			if err != nil {
				// not a number
				return 0, false
			}
			result = operation(result, n)
			state = seekingOp

		case seekingOp:
			switch word {
			case "plus":
				operation = add
			case "minus":
				operation = subtract
			case "multiplied":
				fallthrough
			case "divided":
				if len(fields) == 0 || fields[0] != "by" {
					// malformed operation
					return 0, false
				}
				fields = fields[1:]
				if word == "multiplied" {
					operation = multiply
				} else {
					operation = divide
				}
			default:
				// unknown operation
				return 0, false
			}
			state = seekingNum
		}
	}

	if state == seekingNum {
		// incomplete expression
		return 0, false
	}
	return result, true
}

// bench
//
// with regexp
// BenchmarkAnswer 	   44052	     28083 ns/op	    2960 B/op	      60 allocs/op
//
// with fields state machine
// BenchmarkAnswer    299774          4655 ns/op        1680 B/op         31 allocs/op
