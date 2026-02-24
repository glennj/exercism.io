Matrix = require 'matrix'

describe 'matrix', ->
  it 'extract row from one number matrix', ->
    result = Matrix.row "1", 1
    expected = {1}
    assert.are.same expected, result

  it 'can extract row', ->
    result = Matrix.row "1 2\n3 4", 2
    expected = {3, 4}
    assert.are.same expected, result

  it 'extract row where numbers have different widths', ->
    result = Matrix.row "1 2\n10 20", 2
    expected = {10, 20}
    assert.are.same expected, result

  it 'can extract row from non-square matrix with no corresponding column', ->
    result = Matrix.row "1 2 3\n4 5 6\n7 8 9\n8 7 6", 4
    expected = {8, 7, 6}
    assert.are.same expected, result

  it 'extract column from one number matrix', ->
    result = Matrix.column "1", 1
    expected = {1}
    assert.are.same expected, result

  it 'can extract column', ->
    result = Matrix.column "1 2 3\n4 5 6\n7 8 9", 3
    expected = {3, 6, 9}
    assert.are.same expected, result

  it 'can extract column from non-square matrix with no corresponding row', ->
    result = Matrix.column "1 2 3 4\n5 6 7 8\n9 8 7 6", 4
    expected = {4, 8, 6}
    assert.are.same expected, result

  it 'extract column where numbers have different widths', ->
    result = Matrix.column "89 1903 3\n18 3 1\n9 4 800", 2
    expected = {1903, 3, 4}
    assert.are.same expected, result
