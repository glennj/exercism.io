class Change
  @findFewestCoins: (coins, target) ->
    switch
      when target <  0 then throw new Error "target can't be negative"
      when target == 0 then []
      else @_makeChange(target, @_change(target, coins))


  @_change: (amount, coins) ->
    #   Change making algorithm from
    #   http://www.ccs.neu.edu/home/jaa/CSG713.04F/Information/Handouts/dyn_prog.pdf
    #
    #   This function generates two arrays:
    #
    #   C = maps the minimum number of coins required to make change 
    #       for each n from 1 to amount. It is only used internally here.
    #
    #   S = the _first_ coin used to make change for amount n.
    #       This is the return value.

    rng = [1..amount]
    max = Number.MAX_SAFE_INTEGER

    C = [0].concat (max for _ in rng)
    S = [0].concat (null for _ in rng)

    for p in rng
      min = max
      coin = null

      for c in coins
        if c <= p
          if 1 + C[p - c] < min
            min = 1 + C[p - c]
            coin = c
        C[p] = min
        S[p] = coin

    return S


  @_makeChange: (amount, firstCoin) ->
    throw new Error "can't make target with given coins" if firstCoin[amount] is null

    change = []
    while amount > 0
      coin = firstCoin[amount]
      change.push coin
      amount -= coin

    return change


module.exports = Change
