class Binary
  BASE = 2
  
  constructor: (binaryString) ->
    @digits = (Number.parseInt(d, BASE) for d in binaryString)
    @valid = not @digits.some(isNaN)

  toDecimal: ->
    if @valid then @digits.reduce((dec, b) -> dec * BASE + b) else 0

module.exports = Binary
