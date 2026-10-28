const CMDS = ["wink", "double blink", "close your eyes", "jump"]


func commands(number: int):
	var cmds = range(CMDS.size()) \
			.map(func(i): return [i, CMDS[i]]) \
			.filter(func(pair): return is_bit_set(number, pair[0])) \
			.map(func(pair): return pair[1])

	if is_bit_set(number, CMDS.size()):
		cmds.reverse()

	return cmds


func is_bit_set(n, i): return (n >> i) & 1 == 1
