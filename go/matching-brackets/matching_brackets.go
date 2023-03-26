// Package brackets comment ...
package brackets

/* Experimenting with generics */

// Bracket comment ...
func Bracket(input string) bool {
	stack := newStack[rune]()

	for _, c := range input {
		switch c {
		case '(', '[', '{':
			stack.Push(c)

		case ')', ']', '}':
			b, err := stack.Pop()

			switch {
			case err != nil:
				return false
			case
				b == '(' && c != ')',
				b == '[' && c != ']',
				b == '{' && c != '}':
				return false
			}
		}
	}

	return stack.IsEmpty()
}

// bench
// BenchmarkBracket-2   	 1518996	       787.1 ns/op	     192 B/op	      20 allocs/op
