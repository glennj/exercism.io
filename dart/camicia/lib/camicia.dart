typedef Result = Map<String, dynamic>;
typedef Card = String;
typedef Hand = List<Card>;

class Camicia {
  int cardsPlayed = 0;
  int tricks = 0;
  Map<String, bool> seen = {};
  List<Card> pile = [];

  Result simulateGame(Hand playerA, Hand playerB) {
    cardsPlayed = 0;
    tricks = 0;
    pile = [];
    seen = {};
    var current = playerA;
    var other = playerB;

    while (true) {
      var gameState = handState(playerA) + ":" + handState(playerB);
      if (seen.containsKey(gameState)) return result('loop');
      seen[gameState] = true;

      if (current.isEmpty) {
        collectTrick(other);
        return result('finished');
      }

      var card = discard(current);

      if (isPenaltyCard(card)) {
        (current, other) = payPenalty(card, other, current);
        if (other.isEmpty) return result('finished');
      } else {
        (current, other) = (other, current);
      }
    }
  }

  Card discard(Hand player) {
    var card = player.removeAt(0);
    pile.add(card);
    cardsPlayed += 1;
    return card;
  }

  (Hand, Hand) payPenalty(Card card, Hand payer, Hand payee) {
    for (var i = value(card); i > 0; i--) {
      if (payer.isEmpty) break;
      var c = discard(payer);
      if (isPenaltyCard(c)) return payPenalty(c, payee, payer);
    }
    // penalty fully paid
    collectTrick(payee);
    return (payee, payer);
  }

  void collectTrick(Hand player) {
    player.addAll(pile);
    pile = [];
    tricks += 1;
  }

  Result result(status) =>
      {'status': status, 'cards': cardsPlayed, 'tricks': tricks};

  String handState(Hand hand) => hand.map((card) => value(card)).join();

  int value(Card card) =>
      switch (card) { 'A' => 4, 'K' => 3, 'Q' => 2, 'J' => 1, _ => 0 };

  bool isPenaltyCard(Card card) => value(card) > 0;
}
