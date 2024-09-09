Bucket = require './bucket'

Math.gcd = (a, b) -> if b == 0 then a else Math.gcd(b, a % b)

class TwoBucket
  constructor: ({bucketOne, bucketTwo, @goal, startBucket}) ->
    throw new Error 'impossible' if @goal > Math.max bucketOne, bucketTwo
    gcd = Math.gcd bucketOne, bucketTwo
    throw new Error 'impossible' unless gcd == 1 or @goal % gcd == 0

    @first = new Bucket("one", bucketOne)
    @second = new Bucket("two", bucketTwo)
    [@first, @second] = [@second, @first] if startBucket == "two"

  measure: ->
    response = (b1, b2) -> { moves: moves, goalBucket: b1.name, otherBucket: b2.amount }
    moves = 0

    @first.fill()
    moves += 1

    if @second.size == @goal
      @second.fill()
      moves += 1

    loop
      return response(@first, @second) if @first.amount == @goal
      return response(@second, @first) if @second.amount == @goal

      switch
        when @first.isEmpty() then @first.fill()
        when @second.isFull() then @second.empty()
        else @first.pourInto @second
      moves += 1


module.exports = TwoBucket
