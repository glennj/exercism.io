package yacht

func Score(dice Dice, category string) int {
	switch category {
	case "ones":
		return dice.score(1)
	case "twos":
		return dice.score(2)
	case "threes":
		return dice.score(3)
	case "fours":
		return dice.score(4)
	case "fives":
		return dice.score(5)
	case "sixes":
		return dice.score(6)
	case "full house":
		return dice.fullHouse()
	case "four of a kind":
		return dice.fourOfAKind()
	case "little straight":
		return dice.straight(1)
	case "big straight":
		return dice.straight(2)
	case "yacht":
		return dice.yacht()
	case "choice":
		return dice.sum()
	}
	return 0
}
