class Bucket
  constructor: (@name, @size) ->
    @empty()

  isEmpty: -> @amount is 0
  isFull: -> @amount is @size
  currentCapacity: -> @size - @amount

  empty: -> @amount = 0
  fill: -> @amount = @size
  pourInto: (other) ->
    quantity = Math.min @amount, other.currentCapacity()
    @amount -= quantity
    other.amount += quantity

module.exports = Bucket
