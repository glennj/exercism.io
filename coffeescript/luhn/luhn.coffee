class Luhn
  value = [
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
    [0, 2, 4, 6, 8, 1, 3, 5, 7, 9]
  ]

  constructor: (input) ->
    @input = input.replace(/\s/g, '')

  valid: ->
    return false if @input.length < 2
    sum = 0
    double = 0
    for i in [@input.length - 1 .. 0] by -1
      sum += value[double++ % 2][Number.parseInt(@input[i])]
    sum % 10 is 0
    

module.exports = Luhn
