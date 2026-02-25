Yacht = require 'yacht'

describe 'yacht', ->
  it 'Yacht', ->
    result = Yacht.score 'yacht', {5, 5, 5, 5, 5}
    assert.is.equal 50, result

  it 'Not Yacht', ->
    result = Yacht.score 'yacht', {1, 3, 3, 2, 5}
    assert.is.equal 0, result

  it 'Ones', ->
    result = Yacht.score 'ones', {1, 1, 1, 3, 5}
    assert.is.equal 3, result

  it 'Ones, out of order', ->
    result = Yacht.score 'ones', {3, 1, 1, 5, 1}
    assert.is.equal 3, result

  it 'No ones', ->
    result = Yacht.score 'ones', {4, 3, 6, 5, 5}
    assert.is.equal 0, result

  it 'Twos', ->
    result = Yacht.score 'twos', {2, 3, 4, 5, 6}
    assert.is.equal 2, result

  it 'Fours', ->
    result = Yacht.score 'fours', {1, 4, 1, 4, 1}
    assert.is.equal 8, result

  it 'Yacht counted as threes', ->
    result = Yacht.score 'threes', {3, 3, 3, 3, 3}
    assert.is.equal 15, result

  it 'Yacht of 3s counted as fives', ->
    result = Yacht.score 'fives', {3, 3, 3, 3, 3}
    assert.is.equal 0, result

  it 'Fives', ->
    result = Yacht.score 'fives', {1, 5, 3, 5, 3}
    assert.is.equal 10, result

  it 'Sixes', ->
    result = Yacht.score 'sixes', {2, 3, 4, 5, 6}
    assert.is.equal 6, result

  it 'Full house two small, three big', ->
    result = Yacht.score 'full house', {2, 2, 4, 4, 4}
    assert.is.equal 16, result

  it 'Full house three small, two big', ->
    result = Yacht.score 'full house', {5, 3, 3, 5, 3}
    assert.is.equal 19, result

  it 'Two pair is not a full house', ->
    result = Yacht.score 'full house', {2, 2, 4, 4, 5}
    assert.is.equal 0, result

  it 'Four of a kind is not a full house', ->
    result = Yacht.score 'full house', {1, 4, 4, 4, 4}
    assert.is.equal 0, result

  it 'Yacht is not a full house', ->
    result = Yacht.score 'full house', {2, 2, 2, 2, 2}
    assert.is.equal 0, result

  it 'Four of a Kind', ->
    result = Yacht.score 'four of a kind', {6, 6, 4, 6, 6}
    assert.is.equal 24, result

  it 'Yacht can be scored as Four of a Kind', ->
    result = Yacht.score 'four of a kind', {3, 3, 3, 3, 3}
    assert.is.equal 12, result

  it 'Full house is not Four of a Kind', ->
    result = Yacht.score 'four of a kind', {3, 3, 3, 5, 5}
    assert.is.equal 0, result

  it 'Little Straight', ->
    result = Yacht.score 'little straight', {3, 5, 4, 1, 2}
    assert.is.equal 30, result

  it 'Little Straight as Big Straight', ->
    result = Yacht.score 'big straight', {1, 2, 3, 4, 5}
    assert.is.equal 0, result

  it 'Four in order but not a little straight', ->
    result = Yacht.score 'little straight', {1, 1, 2, 3, 4}
    assert.is.equal 0, result

  it 'No pairs but not a little straight', ->
    result = Yacht.score 'little straight', {1, 2, 3, 4, 6}
    assert.is.equal 0, result

  it 'Minimum is 1, maximum is 5, but not a little straight', ->
    result = Yacht.score 'little straight', {1, 1, 3, 4, 5}
    assert.is.equal 0, result

  it 'Big Straight', ->
    result = Yacht.score 'big straight', {4, 6, 2, 5, 3}
    assert.is.equal 30, result

  it 'Big Straight as little straight', ->
    result = Yacht.score 'little straight', {6, 5, 4, 3, 2}
    assert.is.equal 0, result

  it 'No pairs but not a big straight', ->
    result = Yacht.score 'big straight', {6, 5, 4, 3, 1}
    assert.is.equal 0, result

  it 'Choice', ->
    result = Yacht.score 'choice', {3, 3, 5, 6, 6}
    assert.is.equal 23, result

  it 'Yacht as choice', ->
    result = Yacht.score 'choice', {2, 2, 2, 2, 2}
    assert.is.equal 10, result
