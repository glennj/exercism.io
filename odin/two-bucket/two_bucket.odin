package two_bucket

import "core:math"

Result :: struct {
	moves:        int,
	goal_bucket:  string,
	other_bucket: int,
}

measure :: proc(bucket_one, bucket_two, goal: int, start_bucket: string) -> (Result, bool) {
	if !is_valid(bucket_one, bucket_two, goal) {
		return Result{}, false
	}

	b1 := Bucket{"one", bucket_one, 0}
	b2 := Bucket{"two", bucket_two, 0}

	if start_bucket == "two" { b2, b1 = b1, b2 }

	return solve(&b1, &b2, goal), true
}

@(private)
is_valid :: proc(size1, size2, goal: int) -> bool {
	// is the goal too big for the given buckets?
	if goal > math.max(size1, size2) { return false }

	// is the goal satisfiable?
	divisor := math.gcd(size1, size2)
	return divisor == 1 || goal % divisor == 0
}

@(private)
solve :: proc(start, other: ^Bucket, goal: int) -> Result {
	moves := 0

	fill(start)
	moves += 1

	if other.size == goal && start.size != goal {
		fill(other)
		moves += 1
	}

	for {
		if start.amount == goal { return Result{moves, start.name, other.amount} }
		if other.amount == goal { return Result{moves, other.name, start.amount} }

		switch {
		case is_empty(start^): fill(start)
		case is_full(other^):  empty(other)
		case:                  pour_into(start, other)
		}
		moves += 1
	}
}
