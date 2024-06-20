class Yacht {
  static score(dice, category) {
    return this.new(dice).score(category)
  }

  construct new(dice) {
    _dice = dice[0..-1]
    _dice.sort()
    _counts = {}
    _sum = 0
    for (d in dice) {
      _sum = _sum + d
      _counts[d] = (_counts[d] || 0) + 1
    }
  }

  score(category) {
    if (category == "ones")            return count(1)
    if (category == "twos")            return count(2) * 2
    if (category == "threes")          return count(3) * 3
    if (category == "fours")           return count(4) * 4
    if (category == "fives")           return count(5) * 5
    if (category == "sixes")           return count(6) * 6
    if (category == "full house")      return isFullHouse ? _sum : 0
    if (category == "four of a kind")  return 4 * fourOfAKind
    if (category == "little straight") return isStraight(1) ? 30 : 0
    if (category == "big straight")    return isStraight(2) ? 30 : 0
    if (category == "yacht")           return _counts.count == 1 ? 50 : 0
    if (category == "choice")          return _sum
    Fiber.abort("invalid category")
  }

  count(die) { _counts[die] || 0 }

  isStraight(start) {
    for (i in 0..4) {
      if (_dice[i] != i + start) {
        return false
      }
    }
    return true
  }

  isFullHouse {
    return _counts.count == 2 && _counts.values.contains(3)
  }

  fourOfAKind {
    for (entry in _counts) {
      if (entry.value >= 4) {
        return entry.key
      }
    }
    return 0
  }
}
