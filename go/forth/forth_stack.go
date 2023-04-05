package forth

import (
	"container/list"
	"errors"
)

var (
	ErrEmpty            = errors.New("stack is empty")
	ErrUnderflow        = errors.New("not enough values on the stack")
	ErrUnknownOperation = errors.New("unknown operation")
	ErrDivZero          = errors.New("divide by zero")
)

type ForthStack struct {
	deque *list.List
}

func NewForthStack() *ForthStack {
	return &ForthStack{deque: list.New()}
}

func (s *ForthStack) ToSlice() []int {
	result := make([]int, 0, s.deque.Len())
	for elem := s.deque.Front(); elem != nil; elem = elem.Next() {
		result = append(result, elem.Value.(int))
	}
	return result
}

func (s *ForthStack) want(n int) error {
	if n > 0 {
		size := s.deque.Len()
		if size == 0 {
			return ErrEmpty
		}
		if size < n {
			return ErrUnderflow
		}
	}
	return nil
}

func (s *ForthStack) Push(n int) {
	s.deque.PushBack(n)
}

func (s *ForthStack) Pop() (int, error) {
	elem := s.deque.Back()
	if elem == nil {
		return 0, ErrEmpty
	}
	return s.deque.Remove(elem).(int), nil
}

func (s *ForthStack) ArithmeticOp(op string) error {
	err := s.want(2)
	if err != nil {
		return err
	}
	b, _ := s.Pop()
	a, _ := s.Pop()

	switch op {
	case "+":
		s.Push(a + b)
	case "-":
		s.Push(a - b)
	case "*":
		s.Push(a * b)
	case "/":
		if b == 0 {
			return ErrDivZero
		}
		s.Push(a / b)
	default:
		return ErrUnknownOperation
	}
	return nil
}

func (s *ForthStack) Dup() error {
	a, err := s.Pop()
	if err != nil {
		return err
	}

	s.Push(a)
	s.Push(a)
	return nil
}

func (s *ForthStack) Drop() error {
	_, err := s.Pop()
	return err
}

func (s *ForthStack) Over() error {
	err := s.want(2)
	if err != nil {
		return err
	}
	b, _ := s.Pop()
	a, _ := s.Pop()

	s.Push(a)
	s.Push(b)
	s.Push(a)
	return nil
}

func (s *ForthStack) Swap() error {
	err := s.want(2)
	if err != nil {
		return err
	}
	b, _ := s.Pop()
	a, _ := s.Pop()

	s.Push(b)
	s.Push(a)
	return nil
}
