LineUp = require 'line_up'

describe 'line-up', ->
  it 'format smallest non-exceptional ordinal numeral 4', ->
    result = LineUp.format 'Gianna', 4
    expected = 'Gianna, you are the 4th customer we serve today. Thank you!'
    assert.are.equal expected, result

  pending 'format greatest single digit non-exceptional ordinal numeral 9', ->
    result = LineUp.format 'Maarten', 9
    expected = 'Maarten, you are the 9th customer we serve today. Thank you!'
    assert.are.equal expected, result

  pending 'format non-exceptional ordinal numeral 5', ->
    result = LineUp.format 'Petronila', 5
    expected = 'Petronila, you are the 5th customer we serve today. Thank you!'
    assert.are.equal expected, result

  pending 'format non-exceptional ordinal numeral 6', ->
    result = LineUp.format 'Attakullakulla', 6
    expected = 'Attakullakulla, you are the 6th customer we serve today. Thank you!'
    assert.are.equal expected, result

  pending 'format non-exceptional ordinal numeral 7', ->
    result = LineUp.format 'Kate', 7
    expected = 'Kate, you are the 7th customer we serve today. Thank you!'
    assert.are.equal expected, result

  pending 'format non-exceptional ordinal numeral 8', ->
    result = LineUp.format 'Maximiliano', 8
    expected = 'Maximiliano, you are the 8th customer we serve today. Thank you!'
    assert.are.equal expected, result

  pending 'format exceptional ordinal numeral 1', ->
    result = LineUp.format 'Mary', 1
    expected = 'Mary, you are the 1st customer we serve today. Thank you!'
    assert.are.equal expected, result

  pending 'format exceptional ordinal numeral 2', ->
    result = LineUp.format 'Haruto', 2
    expected = 'Haruto, you are the 2nd customer we serve today. Thank you!'
    assert.are.equal expected, result

  pending 'format exceptional ordinal numeral 3', ->
    result = LineUp.format 'Henriette', 3
    expected = 'Henriette, you are the 3rd customer we serve today. Thank you!'
    assert.are.equal expected, result

  pending 'format smallest two digit non-exceptional ordinal numeral 10', ->
    result = LineUp.format 'Alvarez', 10
    expected = 'Alvarez, you are the 10th customer we serve today. Thank you!'
    assert.are.equal expected, result

  pending 'format non-exceptional ordinal numeral 11', ->
    result = LineUp.format 'Jacqueline', 11
    expected = 'Jacqueline, you are the 11th customer we serve today. Thank you!'
    assert.are.equal expected, result

  pending 'format non-exceptional ordinal numeral 12', ->
    result = LineUp.format 'Juan', 12
    expected = 'Juan, you are the 12th customer we serve today. Thank you!'
    assert.are.equal expected, result

  pending 'format non-exceptional ordinal numeral 13', ->
    result = LineUp.format 'Patricia', 13
    expected = 'Patricia, you are the 13th customer we serve today. Thank you!'
    assert.are.equal expected, result

  pending 'format exceptional ordinal numeral 21', ->
    result = LineUp.format 'Washi', 21
    expected = 'Washi, you are the 21st customer we serve today. Thank you!'
    assert.are.equal expected, result

  pending 'format exceptional ordinal numeral 62', ->
    result = LineUp.format 'Nayra', 62
    expected = 'Nayra, you are the 62nd customer we serve today. Thank you!'
    assert.are.equal expected, result

  pending 'format exceptional ordinal numeral 100', ->
    result = LineUp.format 'John', 100
    expected = 'John, you are the 100th customer we serve today. Thank you!'
    assert.are.equal expected, result

  pending 'format exceptional ordinal numeral 101', ->
    result = LineUp.format 'Zeinab', 101
    expected = 'Zeinab, you are the 101st customer we serve today. Thank you!'
    assert.are.equal expected, result

  pending 'format non-exceptional ordinal numeral 112', ->
    result = LineUp.format 'Knud', 112
    expected = 'Knud, you are the 112th customer we serve today. Thank you!'
    assert.are.equal expected, result

  pending 'format exceptional ordinal numeral 123', ->
    result = LineUp.format 'Yma', 123
    expected = 'Yma, you are the 123rd customer we serve today. Thank you!'
    assert.are.equal expected, result
