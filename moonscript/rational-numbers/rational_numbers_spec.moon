rational = require 'rational_numbers'

describe 'rational-numbers', ->
  -- ----------------------------------------
  -- Why do we need to test "approximately equal"?
  -- See https://0.30000000000000004.com
  epsilon = 1e-9
  is_close_to = (state, arguments) ->
    {a, b} = arguments
    math.abs(a - b) <= epsilon

  say = require 'say'
  say\set 'assertion.approx_equal.positive', "Expected %s and %s to be within #{epsilon}"
  say\set 'assertion.approx_equal.negative', "Expected %s and %s not to be within #{epsilon}"
  assert\register 'assertion', 'approx_equal', is_close_to, 'assertion.approx_equal.positive', 'assertion.approx_equal.negative'
  -- ----------------------------------------

  describe 'Arithmetic', ->
    describe 'Addition', ->
      it 'Add two positive rational numbers', ->
        result = rational.add {1, 2}, {2, 3}
        expected = {7, 6}
        assert.are.same expected, result

      pending 'Add a positive rational number and a negative rational number', ->
        result = rational.add {1, 2}, {-2, 3}
        expected = {-1, 6}
        assert.are.same expected, result

      pending 'Add two negative rational numbers', ->
        result = rational.add {-1, 2}, {-2, 3}
        expected = {-7, 6}
        assert.are.same expected, result

      pending 'Add a rational number to its additive inverse', ->
        result = rational.add {1, 2}, {-1, 2}
        expected = {0, 1}
        assert.are.same expected, result

    describe 'Subtraction', ->
      pending 'Subtract two positive rational numbers', ->
        result = rational.sub {1, 2}, {2, 3}
        expected = {-1, 6}
        assert.are.same expected, result

      pending 'Subtract a positive rational number and a negative rational number', ->
        result = rational.sub {1, 2}, {-2, 3}
        expected = {7, 6}
        assert.are.same expected, result

      pending 'Subtract two negative rational numbers', ->
        result = rational.sub {-1, 2}, {-2, 3}
        expected = {1, 6}
        assert.are.same expected, result

      pending 'Subtract a rational number from itself', ->
        result = rational.sub {1, 2}, {1, 2}
        expected = {0, 1}
        assert.are.same expected, result

    describe 'Multiplication', ->
      pending 'Multiply two positive rational numbers', ->
        result = rational.mul {1, 2}, {2, 3}
        expected = {1, 3}
        assert.are.same expected, result

      pending 'Multiply a negative rational number by a positive rational number', ->
        result = rational.mul {-1, 2}, {2, 3}
        expected = {-1, 3}
        assert.are.same expected, result

      pending 'Multiply two negative rational numbers', ->
        result = rational.mul {-1, 2}, {-2, 3}
        expected = {1, 3}
        assert.are.same expected, result

      pending 'Multiply a rational number by its reciprocal', ->
        result = rational.mul {1, 2}, {2, 1}
        expected = {1, 1}
        assert.are.same expected, result

      pending 'Multiply a rational number by 1', ->
        result = rational.mul {1, 2}, {1, 1}
        expected = {1, 2}
        assert.are.same expected, result

      pending 'Multiply a rational number by 0', ->
        result = rational.mul {1, 2}, {0, 1}
        expected = {0, 1}
        assert.are.same expected, result

    describe 'Division', ->
      pending 'Divide two positive rational numbers', ->
        result = rational.div {1, 2}, {2, 3}
        expected = {3, 4}
        assert.are.same expected, result

      pending 'Divide a positive rational number by a negative rational number', ->
        result = rational.div {1, 2}, {-2, 3}
        expected = {-3, 4}
        assert.are.same expected, result

      pending 'Divide two negative rational numbers', ->
        result = rational.div {-1, 2}, {-2, 3}
        expected = {3, 4}
        assert.are.same expected, result

      pending 'Divide a rational number by 1', ->
        result = rational.div {1, 2}, {1, 1}
        expected = {1, 2}
        assert.are.same expected, result

  describe 'Absolute value', ->
    pending 'Absolute value of a positive rational number', ->
      result = rational.abs {1, 2}
      assert.are.same {1, 2}, result

    pending 'Absolute value of a positive rational number with negative numerator and denominator', ->
      result = rational.abs {-1, -2}
      assert.are.same {1, 2}, result

    pending 'Absolute value of a negative rational number', ->
      result = rational.abs {-1, 2}
      assert.are.same {1, 2}, result

    pending 'Absolute value of a negative rational number with negative denominator', ->
      result = rational.abs {1, -2}
      assert.are.same {1, 2}, result

    pending 'Absolute value of zero', ->
      result = rational.abs {0, 1}
      assert.are.same {0, 1}, result

    pending 'Absolute value of a rational number is reduced to lowest terms', ->
      result = rational.abs {2, 4}
      assert.are.same {1, 2}, result

  describe 'Exponentiation of a rational number', ->
    pending 'Raise a positive rational number to a positive integer power', ->
      result = rational.exprational {1, 2}, 3
      assert.are.same {1, 8}, result

    pending 'Raise a negative rational number to a positive integer power', ->
      result = rational.exprational {-1, 2}, 3
      assert.are.same {-1, 8}, result

    pending 'Raise a positive rational number to a negative integer power', ->
      result = rational.exprational {3, 5}, -2
      assert.are.same {25, 9}, result

    pending 'Raise a negative rational number to an even negative integer power', ->
      result = rational.exprational {-3, 5}, -2
      assert.are.same {25, 9}, result

    pending 'Raise a negative rational number to an odd negative integer power', ->
      result = rational.exprational {-3, 5}, -3
      assert.are.same {-125, 27}, result

    pending 'Raise zero to an integer power', ->
      result = rational.exprational {0, 1}, 5
      assert.are.same {0, 1}, result

    pending 'Raise one to an integer power', ->
      result = rational.exprational {1, 1}, 4
      assert.are.same {1, 1}, result

    pending 'Raise a positive rational number to the power of zero', ->
      result = rational.exprational {1, 2}, 0
      assert.are.same {1, 1}, result

    pending 'Raise a negative rational number to the power of zero', ->
      result = rational.exprational {-1, 2}, 0
      assert.are.same {1, 1}, result

  describe 'Exponentiation of a real number to a rational number', ->
    pending 'Raise a real number to a positive rational number', ->
      result = rational.expreal 8, {4, 3}
      assert.approx_equal 16.0, result

    pending 'Raise a real number to a negative rational number', ->
      result = rational.expreal 9, {-1, 2}
      assert.approx_equal 0.33333333333333, result

    pending 'Raise a real number to a zero rational number', ->
      result = rational.expreal 2, {0, 1}
      assert.approx_equal 1.0, result

  describe 'Reduction to lowest terms', ->
    pending 'Reduce a positive rational number to lowest terms', ->
      result = rational.reduce {2, 4}
      assert.are.same {1, 2}, result

    pending 'Reduce places the minus sign on the numerator', ->
      result = rational.reduce {3, -4}
      assert.are.same {-3, 4}, result

    pending 'Reduce a negative rational number to lowest terms', ->
      result = rational.reduce {-4, 6}
      assert.are.same {-2, 3}, result

    pending 'Reduce a rational number with a negative denominator to lowest terms', ->
      result = rational.reduce {3, -9}
      assert.are.same {-1, 3}, result

    pending 'Reduce zero to lowest terms', ->
      result = rational.reduce {0, 6}
      assert.are.same {0, 1}, result

    pending 'Reduce an integer to lowest terms', ->
      result = rational.reduce {-14, 7}
      assert.are.same {-2, 1}, result

    pending 'Reduce one to lowest terms', ->
      result = rational.reduce {13, 13}
      assert.are.same {1, 1}, result
