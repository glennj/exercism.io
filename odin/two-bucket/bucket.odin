package two_bucket

import "core:math"

Bucket :: struct {
	name:   string,
	size:   int,
	amount: int,
}

// these procs take a concrete Bucket parameter

is_full :: proc(b: Bucket) -> bool { return b.amount == b.size }

is_empty :: proc(b: Bucket) -> bool { return b.amount == 0 }

capacity :: proc(b: Bucket) -> int { return b.size - b.amount }

// while these take a pointer to a Bucket: updates happen here

fill :: proc(b: ^Bucket) { b.amount = b.size }

empty :: proc(b: ^Bucket) { b.amount = 0 }

pour_into :: proc(b: ^Bucket, other: ^Bucket) {
	quantity := math.min(b.amount, capacity(other^))
	b.amount -= quantity
	other.amount += quantity
}
