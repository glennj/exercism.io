package forth

import (
	"fmt"
	"strconv"
	"strings"
)

func Forth(input []string) ([]int, error) {
	stack := NewForthStack()
	macros := NewMacros()

	for _, line := range input {
		err := processLine(strings.ToUpper(line), stack, macros)
		if err != nil {
			return nil, fmt.Errorf("forth: %w", err)
		}
	}

	return stack.ToSlice(), nil
}

func processLine(line string, stack *ForthStack, macros Macros) error {
	var (
		tokens = strings.Fields(line)
		token  string
		err    error
		n      int
	)

Loop:
	for len(tokens) > 0 {
		token, tokens = tokens[0], tokens[1:]

		if macro, ok := macros.Get(token); ok {
			tokens = append(macro, tokens...)
			continue
		}

		switch token {
		case ":":
			err = macros.Record(tokens)
			if err == nil {
				break Loop
			}
		case "+", "-", "*", "/":
			err = stack.ArithmeticOp(token)
		case "DUP":
			err = stack.Dup()
		case "DROP":
			err = stack.Drop()
		case "SWAP":
			err = stack.Swap()
		case "OVER":
			err = stack.Over()
		default:
			n, err = strconv.Atoi(token)
			if err == nil {
				stack.Push(n)
			} else {
				err = ErrUnknownOperation
			}
		}

		if err != nil {
			return fmt.Errorf("processing token '%s': %w", token, err)
		}
	}
	return nil
}

// benchmarked
//
// using my hand-stiched stack
// BenchmarkForth     28886             43342 ns/op           10480 B/op        359 allocs/op
//
// using container/list as a stack
// BenchmarkForth     21145             52534 ns/op           18224 B/op        472 allocs/op
