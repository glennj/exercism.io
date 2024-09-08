class DifferenceOfSquares
  identity = (x) -> x
  square = (x) -> x * x
  
  calcSum = (n, inner, outer) ->
    sum = 0
    for x in [1..n]
      sum += inner(x)
    outer(sum)

  @squareOfSum: (number)  -> calcSum(number, identity, square)
  @sumOfSquares: (number) -> calcSum(number, square, identity)
  
  @differenceOfSquares: (number) ->
    Math.abs(@sumOfSquares(number) - @squareOfSum(number))

module.exports = DifferenceOfSquares
