import classify from require 'perfect_numbers'

describe 'perfect-numbers:', ->
  describe 'Perfect numbers:', ->
    it 'Smallest perfect number is classified correctly', ->
      assert.are.equal 'perfect', classify 6

    pending 'Medium perfect number is classified correctly', ->
      assert.are.equal 'perfect', classify 28

    pending 'Large perfect number is classified correctly', ->
      assert.are.equal 'perfect', classify 33550336

  describe 'Abundant numbers:', ->
    pending 'Smallest abundant number is classified correctly', ->
      assert.are.equal 'abundant', classify 12

    pending 'Medium abundant number is classified correctly', ->
      assert.are.equal 'abundant', classify 30

    pending 'Large abundant number is classified correctly', ->
      assert.are.equal 'abundant', classify 33550335

    pending 'Perfect square abundant number is classified correctly', ->
      assert.are.equal 'abundant', classify 196

  describe 'Deficient numbers:', ->
    pending 'Smallest prime deficient number is classified correctly', ->
      assert.are.equal 'deficient', classify 2

    pending 'Smallest non-prime deficient number is classified correctly', ->
      assert.are.equal 'deficient', classify 4

    pending 'Medium deficient number is classified correctly', ->
      assert.are.equal 'deficient', classify 32

    pending 'Large deficient number is classified correctly', ->
      assert.are.equal 'deficient', classify 33550337

    pending 'Edge case (no factors other than itself) is classified correctly', ->
      assert.are.equal 'deficient', classify 1

  describe 'Invalid inputs:', ->
    pending 'Zero is rejected (as it is not a positive integer)', ->
      func = -> classify 0
      assert.has.error func, 'Classification is only possible for positive integers.'

    pending 'Negative integer is rejected (as it is not a positive integer)', ->
      func = -> classify -1
      assert.has.error func, 'Classification is only possible for positive integers.'
