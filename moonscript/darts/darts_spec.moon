darts = require 'darts'

describe 'darts:', ->
  it 'Missed target', ->
    result = darts.score -9, 9
    assert.are.equal 0, result

  it 'On the outer circle', ->
    result = darts.score 0, 10
    assert.are.equal 1, result

  it 'On the middle circle', ->
    result = darts.score -5, 0
    assert.are.equal 5, result

  it 'On the inner circle', ->
    result = darts.score 0, -1
    assert.are.equal 10, result

  it 'Exactly on center', ->
    result = darts.score 0, 0
    assert.are.equal 10, result

  it 'Near the center', ->
    result = darts.score -0.1, -0.1
    assert.are.equal 10, result

  it 'Just within the inner circle', ->
    result = darts.score 0.7, 0.7
    assert.are.equal 10, result

  it 'Just outside the inner circle', ->
    result = darts.score 0.8, -0.8
    assert.are.equal 5, result

  it 'Just within the middle circle', ->
    result = darts.score -3.5, 3.5
    assert.are.equal 5, result

  it 'Just outside the middle circle', ->
    result = darts.score -3.6, -3.6
    assert.are.equal 1, result

  it 'Just within the outer circle', ->
    result = darts.score -7.0, 7.0
    assert.are.equal 1, result

  it 'Just outside the outer circle', ->
    result = darts.score 7.1, -7.1
    assert.are.equal 0, result

  it 'Asymmetric position between the inner and middle circles', ->
    result = darts.score 0.5, -4
    assert.are.equal 5, result

