import p from require 'moon'

value = (card) ->
  val = switch card
    when 'A' then 4
    when 'K' then 3
    when 'Q' then 2
    when 'J' then 1
    else 0
  -- p "-- value of #{card} is #{val}"
  val

hand_state = (hand) -> table.concat [value c for c in *hand]

game_state = (hands) ->
  s = "#{hand_state hands.A},#{hand_state hands.B}"
  -- p {:hands, :s}
  s

detect_loop = (hist, state) ->
  for s in *hist
    if s == state
      return true
  false

{
  simulateGame: (playerA, playerB) ->
    seen = {}
    pile = {}
    result = status: 'ongoing', cards: 0, tricks: 0
    hands = A: playerA, B: playerB
    current, other = 'A', 'B'

    collect_trick = (who) ->
      p "#{who} gets trick of #{#pile}"
      table.insert hands[who], c for c in *pile
      pile = {}
      result.tricks += 1

    discard = (who) ->
      card = table.remove hands[who], 1
      table.insert pile, card
      result.cards += 1
      p "#{who} adds to pile #{card}"
      card

    while true
      state = game_state hands
      p {:state, :hands, :pile, :current, :result}
      if seen[state]
        result.status = 'loop'
        return result
      seen[state] = true

      if #hands[current] == 0
        p "#{current} is out"
        collect_trick other
        result.status = 'finished'
        return result

      card = discard current

      if value(card) == 0
        current, other = other, current
        continue

      -- paying a penalty
      to_pay = value(card)
      p "penalty of #{value card}"
      for i = value(card), 1, -1
        if #hands[other] == 0
          p "#{other} is out"
          collect_trick current
          current, other = other, current
          break
        else
          c = discard other
          if value(c) == 0
            -- if the payer fully paid the penalty
            collect_trick current if i == 1
          else
            -- put the card back into other's hand and loop back up
            table.insert hands[other], 1, table.remove(pile)
            current, other = other, current
            break

}
