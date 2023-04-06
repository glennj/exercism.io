package queenattack

import "errors"

var (
	ErrInvalidPosition = errors.New("invalid position")
	ErrSamePosition    = errors.New("queens cannot occupy the same position")
)

func CanQueenAttack(whitePosition, blackPosition string) (bool, error) {
	var (
		bx, by, wx, wy int
		ok             bool
	)

	if bx, by, ok = coords(blackPosition); !ok {
		return false, ErrInvalidPosition
	}
	if wx, wy, ok = coords(whitePosition); !ok {
		return false, ErrInvalidPosition
	}
	if bx == wx && by == wy {
		return false, ErrSamePosition
	}

	return (bx == wx || by == wy || abs(bx-wx) == abs(by-wy)), nil
}

func coords(pos string) (int, int, bool) {
	if len(pos) != 2 || pos[0] < 'a' || pos[0] > 'h' || pos[1] < '1' || pos[1] > '8' {
		return 0, 0, false
	}

	return int(pos[0] - 'a'), int(pos[1] - '1'), true
}

func abs(n int) int {
	if n < 0 {
		n = -n
	}
	return n
}

// bench
// BenchmarkCanQueenAttack-2   	16563265	        75.09 ns/op	       0 B/op	       0 allocs/op
