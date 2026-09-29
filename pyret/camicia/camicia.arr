#| This very recursive and mutable solution is also very slow.

$ time pyret camicia-test.arr
...
Looks shipshape, all 28 tests passed, mate!

real    0m18.076s
user    0m19.317s
sys     0m0.403s

|#

use context starter2024

include string-dict
import lists as L

provide: simulate-game end

values = [string-dict: 'A', 4, 'K', 3, 'Q', 2, 'J', 1]

value = lam(card):
  cases(Option) values.get(card):
    | some(val) => val
    | none => 0
  end
end

is-payment-card = lam(card): value(card) > 0 end

# ---------------------------------------------------------
fun simulate-game(player-a, player-b):
  hands = [mutable-string-dict: 'A', player-a, 'B', player-b]
  # a bunch of mutable variables
  var current = 'A'
  var other = 'B'
  var seen = [list: ]
  var pile = [list: ]
  var cards = 0
  var tricks = 0

  collect-trick = lam(player):
    # variable assignments are _statements_ not expressions.
    # multiple statements need to be wrapped in a block.
    block:
      _ = hands.set-now(player, L.append(hands.get-value-now(player), pile.reverse()))
      pile := [list: ]
      tricks := tricks + 1
    end
  end

  discard = lam(player):
    block:
      splt = hands.get-value-now(player).split-at(1)
      card = splt.prefix.get(0)
      _ = hands.set-now(player, splt.suffix)
      pile := pile.push(card)
      cards := cards + 1
      card
    end
  end

  rec pay-penalty = lam(card, payer, payee):
    rec do-pay = lam(n):
      ask:
        | (n == 0) or is-empty(hands.get-value-now(payer)) then:
            _ = collect-trick(payee)
            {payee; payer}
        | otherwise:
            c = discard(payer)
            ask:
              | is-payment-card(c) then: pay-penalty(c, payee, payer)
              | otherwise: do-pay(n - 1)
            end
      end
    end
    do-pay(value(card))
  end

  result = lam(status): {status: status, cards: cards, tricks: tricks} end

  rec do-play = lam():
    state = 
      [list: 'A', 'B']
        .map(lam(player): hands.get-value-now(player).map(value).join-str('') end)
        .join-str(':')

    ask:
      | seen.member(state) then: result('loop')
      | otherwise:
          block:
            seen := seen.push(state)
            ask:
              | is-empty(hands.get-value-now(current)) then:
                  _ = collect-trick(current)
                  result('finished')
              | otherwise:
                  card = discard(current)
                  ask:
                    | is-payment-card(card) then:
                        block:
                          {c; o} = pay-penalty(card, other, current)
                          current := c
                          other := o
                          ask:
                            | is-empty(hands.get-value-now(other)) then:
                                result('finished')
                            | otherwise: do-play()
                          end
                        end
                    | otherwise:
                        block:
                          {c; o} = {other; current}
                          current := c
                          other := o
                          do-play()
                        end
                  end
            end
          end
    end
  end

  do-play()
end
