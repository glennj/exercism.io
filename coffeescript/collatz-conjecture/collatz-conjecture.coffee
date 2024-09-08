class CollatzConjecture
  @steps: (number, steps = 0) ->
    switch
      when number < 1 then throw new Error("Only positive integers are allowed")
      when number == 1 then steps
      when number % 2 == 0 then @steps number / 2, steps + 1
      else @steps number * 3 + 1, steps + 1

module.exports = CollatzConjecture
