package thefarm

import (
	"errors"
	"fmt"
)

// ------------------------------------------------------------
// The SillyNephewError custom error
func NewSillyNephewError(cows int) error {
	return &SillyNephewError{cows}
}

type SillyNephewError struct {
	numCows int
}

func (sne *SillyNephewError) Error() string {
	return fmt.Sprintf("silly nephew, there cannot be %v cows", sne.numCows)
}

// ------------------------------------------------------------
// DivideFood computes the fodder amount per cow for the given cows.
func DivideFood(weightFodder WeightFodder, cows int) (float64, error) {
	w, err := weightFodder.FodderAmount()

	if errors.Is(err, ErrScaleMalfunction) {
		w *= 2
	} else if err != nil {
		return 0, err
	}

	if w < 0 {
		return 0, errors.New("negative fodder")
	}

	if cows == 0 {
		return 0, errors.New("division by zero")
	}

	if cows < 0 {
		return 0, NewSillyNephewError(cows)
	}

	return w / float64(cows), nil
}
