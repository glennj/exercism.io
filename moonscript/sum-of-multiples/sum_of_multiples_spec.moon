SumOfMultiples = require 'sum_of_multiples'

describe 'sum-of-multiples', ->
  it 'no multiples within limit', ->
    result = SumOfMultiples.sum {3, 5}, 1
    assert.are.equal 0, result

  pending 'one factor has multiples within limit', ->
    result = SumOfMultiples.sum {3, 5}, 4
    assert.are.equal 3, result

  pending 'more than one multiple within limit', ->
    result = SumOfMultiples.sum {3}, 7
    assert.are.equal 9, result

  pending 'more than one factor with multiples within limit', ->
    result = SumOfMultiples.sum {3, 5}, 10
    assert.are.equal 23, result

  pending 'each multiple is only counted once', ->
    result = SumOfMultiples.sum {3, 5}, 100
    assert.are.equal 2318, result

  pending 'a much larger limit', ->
    result = SumOfMultiples.sum {3, 5}, 1000
    assert.are.equal 233168, result

  pending 'three factors', ->
    result = SumOfMultiples.sum {7, 13, 17}, 20
    assert.are.equal 51, result

  pending 'factors not relatively prime', ->
    result = SumOfMultiples.sum {4, 6}, 15
    assert.are.equal 30, result

  pending 'some pairs of factors relatively prime and some not', ->
    result = SumOfMultiples.sum {5, 6, 8}, 150
    assert.are.equal 4419, result

  pending 'one factor is a multiple of another', ->
    result = SumOfMultiples.sum {5, 25}, 51
    assert.are.equal 275, result

  pending 'much larger factors', ->
    result = SumOfMultiples.sum {43, 47}, 10000
    assert.are.equal 2203160, result

  pending 'all numbers are multiples of 1', ->
    result = SumOfMultiples.sum {1}, 100
    assert.are.equal 4950, result

  pending 'no factors means an empty sum', ->
    result = SumOfMultiples.sum {}, 10000
    assert.are.equal 0, result

  pending 'the only multiple of 0 is 0', ->
    result = SumOfMultiples.sum {0}, 1
    assert.are.equal 0, result

  pending 'the factor 0 does not affect the sum of multiples of other factors', ->
    result = SumOfMultiples.sum {3, 0}, 4
    assert.are.equal 3, result

  pending 'solutions using include-exclude must extend to cardinality greater than 3', ->
    result = SumOfMultiples.sum {2, 3, 5, 7, 11}, 10000
    assert.are.equal 39614537, result
