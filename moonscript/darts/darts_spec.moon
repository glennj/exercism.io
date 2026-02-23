darts = require 'darts'

describe 'darts', ->
  it 'Missed target', ->
    result = darts.score -9, 9
    assert.are.equal 0, result

  pending 'On the outer circle', ->
    result = darts.score 0, 10
    assert.are.equal 1, result

  pending 'On the middle circle', ->
    result = darts.score -5, 0
    assert.are.equal 5, result

  pending 'On the inner circle', ->
    result = darts.score 0, -1
    assert.are.equal 10, result

  pending 'Exactly on center', ->
    result = darts.score 0, 0
    assert.are.equal 10, result

  pending 'Near the center', ->
    result = darts.score -0.1, -0.1
    assert.are.equal 10, result

  pending 'Just within the inner circle', ->
    result = darts.score 0.7, 0.7
    assert.are.equal 10, result

  pending 'Just outside the inner circle', ->
    result = darts.score 0.8, -0.8
    assert.are.equal 5, result

  pending 'Just within the middle circle', ->
    result = darts.score -3.5, 3.5
    assert.are.equal 5, result

  pending 'Just outside the middle circle', ->
    result = darts.score -3.6, -3.6
    assert.are.equal 1, result

  pending 'Just within the outer circle', ->
    result = darts.score -7.0, 7.0
    assert.are.equal 1, result

  pending 'Just outside the outer circle', ->
    result = darts.score 7.1, -7.1
    assert.are.equal 0, result

  pending 'Asymmetric position between the inner and middle circles', ->
    result = darts.score 0.5, -4
    assert.are.equal 5, result
