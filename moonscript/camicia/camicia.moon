values = A: 4, K: 3, Q: 2, J: 1
value = (card) -> values[card] or 0

is_payment_card = (card) -> value(card) > 0

hand_state = (hand) -> table.concat [value c for c in *hand]


{
  simulateGame: (playerA, playerB) ->
    hands = {A: playerA, B: playerB}
    seen, pile, cards, tricks = {}, {}, 0, 0
    current, other = 'A', 'B'

    -- ------------------------------------------------------------
    -- some helper closure functions
    --
    is_empty = (player) -> #hands[player] == 0

    collect_trick = (player) ->
      table.insert hands[player], c for c in *pile
      pile = {}
      tricks += 1

    discard = (player) ->
      card = table.remove hands[player], 1
      table.insert pile, card
      cards += 1
      card

    pay_penalty = (card, payer, payee) ->
      for i = value(card), 1, -1
        break if is_empty payer
        c = discard payer
        if is_payment_card c
          return pay_penalty c, payee, payer

      -- penalty fully paid, return the next current&other players
      collect_trick payee
      return payee, payer

    result = (status) -> {:status, :cards, :tricks}

    -- ------------------------------------------------------------
    -- the game play
    while true
      state = hand_state(hands.A) .. ':' .. hand_state(hands.B)
      return result 'loop' if seen[state]
      seen[state] = true

      if is_empty current
        collect_trick other
        return result 'finished'

      card = discard current

      if not is_payment_card card
        -- next player's turn
        current, other = other, current
        continue

      current, other = pay_penalty card, other, current
      return result 'finished' if is_empty other
}
