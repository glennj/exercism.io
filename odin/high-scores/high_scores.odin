package high_scores

import "core:slice"
import "core:math/bits"

High_Scores :: struct {
	scores: []int,
}

copy_slice :: proc(s: []int) -> []int {
	copy := make([]int, len(s))
	for i in 0 ..< len(s) {
		copy[i] = s[i]
	}
	return copy[:]
}

new_scores :: proc(initial_values: []int) -> High_Scores {
	return High_Scores{copy_slice(initial_values)}
}

destroy_scores :: proc(s: ^High_Scores) {
	delete(s.scores)
}

scores :: proc(s: High_Scores) -> []int {
	return copy_slice(s.scores)
}

latest :: proc(s: High_Scores) -> int {
	return s.scores[len(s.scores) - 1]
}

personal_best :: proc(s: High_Scores) -> int {
	best := bits.I64_MIN
	for i := 0; i < len(s.scores); i += 1 {
		best = max(best, s.scores[i])
	}
	return best
}

personal_top_three :: proc(s: High_Scores) -> []int {
	copy := scores(s)
	slice.reverse_sort(copy)
	end := min(3, len(copy))
	return copy[0:end]
}
