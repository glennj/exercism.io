package yacht

import "sort"

//lint:file-ignore U1000 only one func will actually be used

type Dice []int

func (dice Dice) freq() (f map[int]int) {
	f = make(map[int]int)
	for _, die := range dice {
		f[die]++
	}
	return
}

func (dice Dice) sort() {
	sort.Slice(dice, func(i, j int) bool {
		return dice[i] <= dice[j]
	})
}

func (dice Dice) yacht() (score int) {
	if len(dice.freq()) == 1 {
		score = 50
	}
	return
}

func (dice Dice) sum() (sum int) {
	for _, die := range dice {
		sum += die
	}
	return
}

func (dice Dice) score(die int) int {
	return dice.freq()[die] * die
}

func (dice Dice) fullHouse() (score int) {
	f := dice.freq()
	for _, count := range f {
		if len(f) == 2 && count == 2 || count == 3 {
			score = dice.sum()
		}
		break
	}
	return
}

func (dice Dice) fourOfAKind() (score int) {
	for die, count := range dice.freq() {
		if count >= 4 {
			score = 4 * die
			break
		}
	}
	return
}

// ------------------------------------------------------------
func (dice Dice) straight(start int) int {
	return dice.straightSort(start)
	//return dice.straightIterate(start)
}

func (dice Dice) straightSort(start int) int {
	dice.sort()
	for i, die := range dice {
		if die != i+start {
			return 0
		}
	}
	return 30
}

func (dice Dice) straightIterate(start int) int {
	f := dice.freq()
	for die := start; die < start+5; die++ {
		if f[die] != 1 {
			return 0
		}
	}
	return 30
}

// bench
//
// straight: sort
// BenchmarkScore    238153              4883 ns/op             832 B/op         32 allocs/op
//
// straight: freq + iterate -- a little leaner but a little slower
// BenchmarkScore    255340              4540 ns/op             384 B/op         16 allocs/op
