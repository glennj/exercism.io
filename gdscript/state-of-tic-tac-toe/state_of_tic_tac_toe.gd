const WIN_MASKS = [
	0b111_000_000,
	0b000_111_000,
	0b000_000_111,
	0b100_100_100,
	0b010_010_010,
	0b001_001_001,
	0b100_010_001,
	0b001_010_100,
]

func gamestate(board):
	board = Array("".join(board).split())
	var count = {"X": 0, "O": 0, " ": 0}
	for c in board:
		count[c] += 1

	var value = func(player):
		var val = 0
		for i in range(board.size()):
			if board[i] == player:
				val |= 1 << i
		return val

	var values = {
		"X": value.call("X"),
		"O": value.call("O")
	}

	var wins = {"X": false, "O": false}
	for player in ["X", "O"]:
		for mask in WIN_MASKS:
			if values[player] & mask == mask:
				wins[player] = true

	if wins.X and count.O >= count.X:     return null
	if wins.O and count.X >= count.O + 1: return null
	if count.X > count.O + 1:             return null
	if count.O > count.X:                 return null

	if wins.X or wins.O:       return "win"
	if count.X + count.O == 9: return "draw"
	return "ongoing"
