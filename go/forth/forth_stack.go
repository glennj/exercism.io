package forth

import (
	"errors"
	"stack"
)

var (
	ErrUnderflow        = errors.New("not enough values on the stack")
	ErrUnknownOperation = errors.New("unknown operation")
	ErrDivZero          = errors.New("divide by zero")
)

type ForthStack struct {
	stack stack.Stack[int]
}

func NewForthStack() *ForthStack {
	return &ForthStack{stack: stack.NewStack[int]()}
}

func (s *ForthStack) ToSlice() []int {
	size := s.stack.Size()
	result := make([]int, size)
	for i := 0; i < size; i++ {
		result[i] = s.stack[i]
	}
	return result
}

func (s *ForthStack) want(n int) error {
	if n > 0 {
		size := s.stack.Size()
		if size == 0 {
			return stack.ErrEmpty
		}
		if size < n {
			return ErrUnderflow
		}
	}
	return nil
}

func (s *ForthStack) Push(n int) {
	s.stack.Push(n)
}

func (s *ForthStack) ArithmeticOp(op string) error {
	err := s.want(2)
	if err != nil {
		return err
	}
	b, err := s.stack.Pop()
	if err != nil {
		return err
	}
	a, err := s.stack.Pop()
	if err != nil {
		return err
	}

	switch op {
	case "+":
		s.stack.Push(a + b)
	case "-":
		s.stack.Push(a - b)
	case "*":
		s.stack.Push(a * b)
	case "/":
		if b == 0 {
			return ErrDivZero
		}
		s.stack.Push(a / b)
	default:
		return ErrUnknownOperation
	}
	return nil
}

func (s *ForthStack) Dup() error {
	err := s.want(1)
	if err != nil {
		return err
	}
	a, err := s.stack.Pop()
	if err != nil {
		return err
	}

	s.stack.Push(a)
	s.stack.Push(a)
	return nil
}

func (s *ForthStack) Drop() error {
	err := s.want(1)
	if err != nil {
		return err
	}
	_, err = s.stack.Pop()
	if err != nil {
		return err
	}
	return nil
}

func (s *ForthStack) Over() error {
	err := s.want(2)
	if err != nil {
		return err
	}
	b, err := s.stack.Pop()
	if err != nil {
		return err
	}
	a, err := s.stack.Pop()
	if err != nil {
		return err
	}

	s.stack.Push(a)
	s.stack.Push(b)
	s.stack.Push(a)
	return nil
}

func (s *ForthStack) Swap() error {
	err := s.want(2)
	if err != nil {
		return err
	}
	b, err := s.stack.Pop()
	if err != nil {
		return err
	}
	a, err := s.stack.Pop()
	if err != nil {
		return err
	}

	s.stack.Push(b)
	s.stack.Push(a)
	return nil
}
