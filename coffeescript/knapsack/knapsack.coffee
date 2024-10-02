###
Translating the pseudocode from https://en.wikipedia.org/wiki/Knapsack_problem
###

class Knapsack
  @maximumValue: ({maximumWeight, items}) ->
    m = new Array(items.length)
    m[0] = new Array(maximumWeight + 1).fill 0

    for {weight, value}, idx in items
      i = idx + 1
      m[i] = new Array(maximumWeight + 1)

      for w in [0..maximumWeight]
        m[i][w] = switch
          when weight > w then m[i - 1][w]
          else Math.max m[i - 1][w], value + m[i - 1][w - weight]

    m[items.length][maximumWeight]


module.exports = Knapsack
