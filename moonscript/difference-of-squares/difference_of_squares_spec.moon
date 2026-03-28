DifferenceOfSquares = require 'difference_of_squares'

describe 'difference-of-squares', ->
  describe 'Square the sum of the numbers up to the given number', ->
    it 'square of sum 1', ->
      result = DifferenceOfSquares.square_of_sum 1
      assert.are.equal 1, result

    pending 'square of sum 5', ->
      result = DifferenceOfSquares.square_of_sum 5
      assert.are.equal 225, result

    pending 'square of sum 100', ->
      result = DifferenceOfSquares.square_of_sum 100
      assert.are.equal 25502500, result

  describe 'Sum the squares of the numbers up to the given number', ->
    pending 'sum of squares 1', ->
      result = DifferenceOfSquares.sum_of_squares 1
      assert.are.equal 1, result

    pending 'sum of squares 5', ->
      result = DifferenceOfSquares.sum_of_squares 5
      assert.are.equal 55, result

    pending 'sum of squares 100', ->
      result = DifferenceOfSquares.sum_of_squares 100
      assert.are.equal 338350, result

  describe 'Subtract sum of squares from square of sums', ->
    pending 'difference of squares 1', ->
      result = DifferenceOfSquares.difference_of_squares 1
      assert.are.equal 0, result

    pending 'difference of squares 5', ->
      result = DifferenceOfSquares.difference_of_squares 5
      assert.are.equal 170, result

    pending 'difference of squares 100', ->
      result = DifferenceOfSquares.difference_of_squares 100
      assert.are.equal 25164150, result
