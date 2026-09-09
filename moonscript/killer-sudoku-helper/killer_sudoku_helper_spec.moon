import combinations from require 'killer_sudoku_helper'

describe 'killer-sudoku-helper:', ->
  describe 'Trivial 1-digit cages:', ->
    it '1', ->
      result = combinations sum: 1, size: 1, exclude: {}
      expected = {{1}}
      assert.are.same expected, result

    it '2', ->
      result = combinations sum: 2, size: 1, exclude: {}
      expected = {{2}}
      assert.are.same expected, result

    it '3', ->
      result = combinations sum: 3, size: 1, exclude: {}
      expected = {{3}}
      assert.are.same expected, result

    it '4', ->
      result = combinations sum: 4, size: 1, exclude: {}
      expected = {{4}}
      assert.are.same expected, result

    it '5', ->
      result = combinations sum: 5, size: 1, exclude: {}
      expected = {{5}}
      assert.are.same expected, result

    it '6', ->
      result = combinations sum: 6, size: 1, exclude: {}
      expected = {{6}}
      assert.are.same expected, result

    it '7', ->
      result = combinations sum: 7, size: 1, exclude: {}
      expected = {{7}}
      assert.are.same expected, result

    it '8', ->
      result = combinations sum: 8, size: 1, exclude: {}
      expected = {{8}}
      assert.are.same expected, result

    it '9', ->
      result = combinations sum: 9, size: 1, exclude: {}
      expected = {{9}}
      assert.are.same expected, result

  it 'Cage with sum 45 contains all digits 1:9', ->
    result = combinations sum: 45, size: 9, exclude: {}
    expected = {{1, 2, 3, 4, 5, 6, 7, 8, 9}}
    assert.are.same expected, result

  it 'Cage with only 1 possible combination', ->
    result = combinations sum: 7, size: 3, exclude: {}
    expected = {{1, 2, 4}}
    assert.are.same expected, result

  it 'Cage with several combinations', ->
    result = combinations sum: 10, size: 2, exclude: {}
    expected = {
      {1, 9},
      {2, 8},
      {3, 7},
      {4, 6},
    }
    assert.are.same expected, result

  it 'Cage with several combinations that is restricted', ->
    result = combinations sum: 10, size: 2, exclude: {1, 4}
    expected = {
      {2, 8},
      {3, 7},
    }
    assert.are.same expected, result

