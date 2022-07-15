class Change {
  static findMinimumCoins(target, coins) {
    if (target < 0) {
      Fiber.abort("target can't be negative")
    }
    if (target == 0 ) {
      return []
    }
    return makeChange(change(coins, target), target)
  }

  /*
   * Change making algorithm from
   * http://www.ccs.neu.edu/home/jaa/CSG713.04F/Information/Handouts/dyn_prog.pdf
   *
   * This function generates two lists, but only returns one:
   *
   * C = the minimum _number of coins_ required to make change
   *     for each n from 1 to amount.
   *
   * S = the _first_ coin used to make change for amount n.
   */
  static change(denominations, valueOfCoins) {
    var C = [0] + List.filled(valueOfCoins, null)
    var S = [0] + List.filled(valueOfCoins, null)

    for (p in 1..valueOfCoins) {
      var min = Num.infinity
      var coin
      for (c in denominations) {
        if (c > p) break
        if (1 + C[p - c] < min) {
          min = 1 + C[p - c]
          coin = c
        }
      }
      C[p] = min
      S[p] = coin
    }
    return S
  }

  static makeChange(changeData, valueOfCoins) {
    var coins = []
    while (valueOfCoins > 0) {
      var coin = changeData[valueOfCoins]
      if (coin == null) Fiber.abort("can't make target with given coins")

      coins.add(coin)
      valueOfCoins = valueOfCoins - coin
    }
    return coins
  }
}
