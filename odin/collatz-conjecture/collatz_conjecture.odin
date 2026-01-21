package collatz_conjecture

steps :: proc(start: int) -> (steps: int, ok: bool) {
	ok = start >= 1
	if ok {
		n := start
		for n > 1 {
			steps += 1
			n = n/2 if n & 1 == 0 else 3*n + 1
		}
	}
	return
}
