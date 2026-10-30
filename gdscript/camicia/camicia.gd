const VALUES = {A = 4, K = 3, Q = 2, J = 1}

func value(card): return VALUES.get(card, 0)

func is_payment_card(card): return value(card) > 0

func hand_state(hand):
	return "".join(hand.map(func (card): return str(value(card))))


func simulate_game(player_a: Array, player_b: Array) -> Dictionary:
	var hands = {A = player_a, B = player_b}
	var seen = {}
	var current = "A"
	var other = "B"

	# -------------------------------------
	# a few closures
	# HOWEVER, GDScript does not have "true" closures:
	#
	# > Local variables are captured by value once, when the lambda is created.
	# > Also, a lambda **cannot reassign an outer local variable**.
	# > After exiting the lambda, **the variable will be unchanged**,
	# > because the lambda capture implicitly shadows it.
	# > However, if you use pass-by-reference data types (arrays, dictionaries,
	# > and objects), then the content changes are shared
	#
	# -- https://docs.godotengine.org/en/stable/tutorials/scripting/gdscript/gdscript_basics.html#lambda-functions
	#
	# Re-assigning an outer variable only persists the new value in the
	# function itself: when the lambda returns, the outer variable's value has
	# not changed. Therefore, outer variables intended for use in a lambda
	# must be stored in a reference type.

	var game_state = {
		pile = [],
		cards = 0,
		tricks = 0
	}

	var collect_trick = func (player):
		hands[player].append_array(game_state.pile)
		game_state.pile = []
		game_state.tricks += 1

	var discard = func (player):
		var card = hands[player].pop_front()
		game_state.pile.append(card)
		game_state.cards += 1
		return card

	var pay_penalty = func (recur, card, payer, payee):
		for i in range(value(card), 0, -1):
			if hands[payer].is_empty(): break

			var c = discard.call(payer)
			if is_payment_card(c):
				return recur.call(recur, c, payee, payer)

		# penalty fully paid
		collect_trick.call(payee)
		return [payee, payer]

	var result = func (status):
		return {
			"status": status,
			"cards": game_state.cards,
			"tricks": game_state.tricks
		}

	# -------------------------------------
	# the game play
	
	while true:
		var state = "%s:%s" % [hands.A, hands.B].map(hand_state)
		if seen.has(state): return result.call("loop") 
		seen[state] = true

		if hands[current].is_empty():
			collect_trick.call(other)
			break

		var card = discard.call(current)

		if is_payment_card(card):
			var players = pay_penalty.call(pay_penalty, card, other, current)
			current = players[0]
			other = players[1]
			if hands[other].is_empty(): break

		else:
			# next player's turn
			var tmp = current
			current = other
			other = tmp

	return result.call("finished")
