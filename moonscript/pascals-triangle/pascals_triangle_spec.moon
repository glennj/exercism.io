PascalsTriangle = require 'pascals_triangle'

describe 'pascals-triangle', ->
  it 'zero rows', ->
    result = PascalsTriangle.rows 0
    expected = {}
    assert.are.same expected, result

  pending 'single row', ->
    result = PascalsTriangle.rows 1
    expected = {{1}}
    assert.are.same expected, result

  pending 'two rows', ->
    result = PascalsTriangle.rows 2
    expected = {
      {1},
      {1, 1},
    }
    assert.are.same expected, result

  pending 'three rows', ->
    result = PascalsTriangle.rows 3
    expected = {
      {1},
      {1, 1},
      {1, 2, 1},
    }
    assert.are.same expected, result

  pending 'four rows', ->
    result = PascalsTriangle.rows 4
    expected = {
      {1},
      {1, 1},
      {1, 2, 1},
      {1, 3, 3, 1},
    }
    assert.are.same expected, result

  pending 'five rows', ->
    result = PascalsTriangle.rows 5
    expected = {
      {1},
      {1, 1},
      {1, 2, 1},
      {1, 3, 3, 1},
      {1, 4, 6, 4, 1},
    }
    assert.are.same expected, result

  pending 'six rows', ->
    result = PascalsTriangle.rows 6
    expected = {
      {1},
      {1, 1},
      {1, 2, 1},
      {1, 3, 3, 1},
      {1, 4, 6, 4, 1},
      {1, 5, 10, 10, 5, 1},
    }
    assert.are.same expected, result

  pending 'ten rows', ->
    result = PascalsTriangle.rows 10
    expected = {
      {1},
      {1, 1},
      {1, 2, 1},
      {1, 3, 3, 1},
      {1, 4, 6, 4, 1},
      {1, 5, 10, 10, 5, 1},
      {1, 6, 15, 20, 15, 6, 1},
      {1, 7, 21, 35, 35, 21, 7, 1},
      {1, 8, 28, 56, 70, 56, 28, 8, 1},
      {1, 9, 36, 84, 126, 126, 84, 36, 9, 1},
    }
    assert.are.same expected, result
