package twobucket

import (
	"errors"
	"fmt"
)

func Solve(sizeBucketOne, sizeBucketTwo, goalAmount int, startBucket string) (string, int, int, error) {
	err := validate(sizeBucketOne, sizeBucketTwo, goalAmount, startBucket)
	if err != nil {
		return "", 0, 0, fmt.Errorf("cannot solve: %w", err)
	}

	var (
		winner        string
		other         int
		moves         = 0
		first, second = initialize(sizeBucketOne, sizeBucketTwo, startBucket)
	)

	// here's where we actually start to solve it
	first.Fill()
	moves++

	if second.Size == goalAmount {
		second.Fill()
		moves++
	}

	for {
		if first.Amount == goalAmount {
			winner = first.Name
			other = second.Amount
			break
		}
		if second.Amount == goalAmount {
			winner = second.Name
			other = first.Amount
			break
		}

		switch {
			case first.IsEmpty(): first.Fill()
			case second.IsFull(): second.Empty()
			default:			  first.PourInto(&second)
		}
		moves++
	}

	return winner, moves, other, nil
}

func initialize(size1, size2 int, start string) (Bucket, Bucket) {
	one := Bucket{Name: "one", Size: size1}
	two := Bucket{Name: "two", Size: size2}

	if start == "two" {
		return two, one
	}
	return one, two
}

func validate(size1, size2, goal int, start string) error {
	if start != "one" && start != "two" {
		return fmt.Errorf("invalid start name %s", start)
	}
	if goal > size1 && goal > size2 {
		return errors.New("goal too big")
	}
	if goal == 0 {
		return errors.New("goal is zero")
	}
	if size1 == 0 || size2 == 0 {
		return errors.New("bucket has zero size")
	}
	gcd := GCD(size1, size2)
	if gcd != 1 && goal%gcd != 0 {
		return errors.New("goal is unattainable")
	}
	return nil
}

func GCD(a, b int) int {
	if b == 0 {
		return a
	}
	return GCD(b, a%b)
}

// bench
// BenchmarkSolve    288946              4340 ns/op            2632 B/op         21 allocs/op
