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


func sum(dice):
	return dice.reduce(func(a, d): return a + d, 0)


func score(dice, category):
	match category:
		ONES, TWOS, THREES, FOURS, FIVES, SIXES:
			return sum(dice.filter(func(d): return d == category))

		FULL_HOUSE:
			if score(dice, YACHT):
				return 0
			# yacht sorts the dice for us
			if dice[0] == dice[1] and dice[2] == dice[4] \
			or dice[0] == dice[2] and dice[3] == dice[4]:
				return sum(dice)
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
			return sum(dice)
