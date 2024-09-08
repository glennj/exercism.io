class BaseConverter  
  base: -> throw new Error 'subclasses responsibility'
  
  constructor: (digitString) ->
    @digits = (Number.parseInt(d, @base()) for d in digitString)
    @valid = not @digits.some(isNaN)

  toDecimal: ->
    # need a fat arrow here to connect @base to this
    if @valid then @digits.reduce((dec, b) => dec * @base() + b) else 0
    

module.exports = BaseConverter
