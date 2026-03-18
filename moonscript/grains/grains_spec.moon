Grains = require 'grains'

describe 'grains', ->
  describe 'returns the number of grains on the square', ->
    it 'grains on square 1', ->
      assert.are.equal 1, Grains.square 1

    pending 'grains on square 2', ->
      assert.are.equal 2, Grains.square 2

    pending 'grains on square 3', ->
      assert.are.equal 4, Grains.square 3

    pending 'grains on square 4', ->
      assert.are.equal 8, Grains.square 4

    pending 'grains on square 16', ->
      assert.are.equal 32768, Grains.square 16

    pending 'grains on square 32', ->
      assert.are.equal 2147483648, Grains.square 32

    pending 'grains on square 64', ->
      assert.are.equal 9223372036854775808, Grains.square 64

    pending 'square 0 is invalid', ->
      assert.has.errors -> Grains.square 0, 'square must be between 1 and 64'

    pending 'negative square is invalid', ->
      assert.has.errors -> Grains.square -1, 'square must be between 1 and 64'

    pending 'square greater than 64 is invalid', ->
      assert.has.errors -> Grains.square 65, 'square must be between 1 and 64'

  pending 'returns the total number of grains on the board', ->
    assert.are.equal 18446744073709551615, Grains.total!
