package letter

// FreqMap records the frequency of each rune in a given text.
type FreqMap map[rune]int

// Frequency counts the frequency of each rune in a given text and returns this
// data as a FreqMap.
func Frequency(text string) FreqMap {
	frequencies := FreqMap{}
	for _, r := range text {
		frequencies[r]++
	}
	return frequencies
}

// ConcurrentFrequency counts the frequency of each rune in the given strings,
// by making use of concurrency.
func ConcurrentFrequency(texts []string) FreqMap {
	result := FreqMap{}

	// This channel is like a message queue to transfer results out of a goroutine.
	// It is fully buffered (i.e. nonblocking)
	ch := make(chan FreqMap, len(texts))

	// Launch a concurrent frequency analyzer for each input string.
	for _, text := range texts {
		go analyzer(text, ch)
	}

	// Then reap each one, in _some_ order.
	for i := 0; i < len(texts); i++ {
		for r, count := range <-ch {
			result[r] += count
		}
	}

	return result
}

func analyzer(s string, ch chan FreqMap) {
	ch <- Frequency(s)
}
