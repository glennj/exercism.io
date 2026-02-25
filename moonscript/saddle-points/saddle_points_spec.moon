import saddle_points from require 'saddle_points'

describe 'saddle-points', ->
  cmp_saddle_points = (a, b) ->
    a.row < b.row or (a.row == b.row and a.column < b.column)

  it 'Can identify single saddle point', ->
    matrix = {
      {9, 8, 7},
      {5, 3, 2},
      {6, 6, 7},
    }
    result = saddle_points matrix
    expected = {{row: 2, column: 1}}
    table.sort result, cmp_saddle_points
    table.sort expected, cmp_saddle_points
    assert.are.same expected, result

  it 'Can identify that empty matrix has no saddle points', ->
    matrix = {{}}
    result = saddle_points matrix
    expected = {}
    table.sort result, cmp_saddle_points
    table.sort expected, cmp_saddle_points
    assert.are.same expected, result

  it 'Can identify lack of saddle points when there are none', ->
    matrix = {
      {1, 2, 3},
      {3, 1, 2},
      {2, 3, 1},
    }
    result = saddle_points matrix
    expected = {}
    table.sort result, cmp_saddle_points
    table.sort expected, cmp_saddle_points
    assert.are.same expected, result

  it 'Can identify multiple saddle points in a column', ->
    matrix = {
      {4, 5, 4},
      {3, 5, 5},
      {1, 5, 4},
    }
    result = saddle_points matrix
    expected = {{row: 1, column: 2}, {row: 2, column: 2}, {row: 3, column: 2}}
    table.sort result, cmp_saddle_points
    table.sort expected, cmp_saddle_points
    assert.are.same expected, result

  it 'Can identify multiple saddle points in a row', ->
    matrix = {
      {6, 7, 8},
      {5, 5, 5},
      {7, 5, 6},
    }
    result = saddle_points matrix
    expected = {{row: 2, column: 1}, {row: 2, column: 2}, {row: 2, column: 3}}
    table.sort result, cmp_saddle_points
    table.sort expected, cmp_saddle_points
    assert.are.same expected, result

  it 'Can identify saddle point in bottom right corner', ->
    matrix = {
      {8, 7, 9},
      {6, 7, 6},
      {3, 2, 5},
    }
    result = saddle_points matrix
    expected = {{row: 3, column: 3}}
    table.sort result, cmp_saddle_points
    table.sort expected, cmp_saddle_points
    assert.are.same expected, result

  it 'Can identify saddle points in a non square matrix', ->
    matrix = {
      {3, 1, 3},
      {3, 2, 4},
    }
    result = saddle_points matrix
    expected = {{row: 1, column: 3}, {row: 1, column: 1}}
    table.sort result, cmp_saddle_points
    table.sort expected, cmp_saddle_points
    assert.are.same expected, result

  it 'Can identify that saddle points in a single column matrix are those with the minimum value', ->
    matrix = {
      {2},
      {1},
      {4},
      {1},
    }
    result = saddle_points matrix
    expected = {{row: 2, column: 1}, {row: 4, column: 1}}
    table.sort result, cmp_saddle_points
    table.sort expected, cmp_saddle_points
    assert.are.same expected, result

  it 'Can identify that saddle points in a single row matrix are those with the maximum value', ->
    matrix = {{2, 5, 3, 5}}
    result = saddle_points matrix
    expected = {{row: 1, column: 2}, {row: 1, column: 4}}
    table.sort result, cmp_saddle_points
    table.sort expected, cmp_saddle_points
    assert.are.same expected, result
