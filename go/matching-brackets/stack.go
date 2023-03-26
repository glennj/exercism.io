package brackets

import "errors"

// ErrEmpty comment ...
var ErrEmpty = errors.New("stack is empty")

// Stack represents a stack of generic elements
type Stack[T any] []T

func newStack[T any]() Stack[T] {
	return Stack[T]{}
}

// Push appends an element to the stack
func (s *Stack[T]) Push(elem T) {
	*s = append(*s, elem)
}

// Pop removes an element from the stack and returns it
func (s *Stack[T]) Pop() (elem T, err error) {
	if s.IsEmpty() {
		err = ErrEmpty
	} else {
		i := len(*s) - 1
		elem = (*s)[i]
		*s = (*s)[:i]
	}
	return
}

// IsEmpty responds true if the stack has no elements
func (s *Stack[T]) IsEmpty() bool {
	return len(*s) == 0
}
