var Values = {"A": 4, "K": 3, "Q": 2, "J": 1}

class Camicia {
  static simulateGame(playerA, playerB) { Camicia.new(playerA, playerB).play }

  construct new(playerA, playerB) {
    _hands = {"A": playerA, "B": playerB}
    _pile = []
    _cardsPlayed = 0
    _tricks = 0
    _history = {}
  }

  play {
    var current = "A"
    var other = "B"

    while (true) {
      if (isLoop) return result("loop")

      if (_hands[current].isEmpty) {
        collectTrick(other)
        return result("finished")
      }

      var card = discard(current)

      if (isPenaltyCard(card)) {
        var players = payPenalty(card, other, current)
        current = players[0]
        other = players[1]
        if (_hands[other].isEmpty) return result("finished")
      } else {
        var tmp = current
        current = other
        other = tmp
      }
    }
  }

  result(status) { {"status": status, "cards": _cardsPlayed, "tricks": _tricks} }

  value(card) { Values[card] != null ? Values[card] : 0 }

  handState(hand) { hand.map {|c| value(c)}.join("") }

  isLoop {
    var state = handState(_hands["A"]) + ":" + handState(_hands["B"])
    if (_history.containsKey(state)) return true
    _history[state] = true
    return false
  }

  collectTrick(player) {
    _hands[player] = _hands[player] + _pile
    _pile = []
    _tricks = _tricks + 1
  }

  discard(player) {
    var card = _hands[player].removeAt(0)
    _pile.add(card)
    _cardsPlayed = _cardsPlayed + 1
    return card
  }

  isPenaltyCard(card) { value(card) > 0 }

  payPenalty(card, payer, payee) {
    for (i in value(card)...0) {
      if (_hands[payer].isEmpty) break
      var c = discard(payer)
      if (isPenaltyCard(c)) return payPenalty(c, payee, payer)
    }
    // penalty is fully paid
    collectTrick(payee)
    return [payee, payer]
  }
}
