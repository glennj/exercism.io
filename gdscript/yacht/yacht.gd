enum {
	YACHT,
	ONES,
	TWOS,
	THREES,
	FOURS,
	FIVES,
	SIXES,
	FULL_HOUSE,
	FOUR_OF_A_KIND,
	LITTLE_STRAIGHT,
	BIG_STRAIGHT,
	CHOICE
}

func single(dice, die):
	return dice.reduce(func(a, d): return a + (d if d == die else 0), 0)

func score(dice, category):
	match category:
		ONES:	return single(dice, 1)
		TWOS:	return single(dice, 2)
		THREES:	return single(dice, 3)
		FOURS:	return single(dice, 4)
		FIVES:	return single(dice, 5)
		SIXES:	return single(dice, 6)

		FULL_HOUSE:
			dice.sort()
			if score(dice, YACHT):
				return 0
			if dice[0] == dice[1] and dice[2] == dice[4] \
			or dice[0] == dice[2] and dice[3] == dice[4]:
				return score(dice, CHOICE)
			return 0

		FOUR_OF_A_KIND:
			dice.sort()
			if dice[0] == dice[3] or dice[1] == dice[4]:
				return 4 * dice[2]
			return 0

		LITTLE_STRAIGHT:
			dice.sort()
			return 30 if dice == [1, 2, 3, 4, 5] else 0

		BIG_STRAIGHT:
			dice.sort()
			return 30 if dice == [2, 3, 4, 5, 6] else 0
			
		YACHT:
			dice.sort()
			return 50 if dice[0] == dice[4] else 0

		CHOICE:
			return dice.reduce(func(a, d): return a + d, 0)
