Scrabble = require 'scrabble_score'

describe 'scrabble-score', ->
  it 'lowercase letter', ->
    result = Scrabble.score 'a'
    assert.are.equal 1, result

  it 'uppercase letter', ->
    result = Scrabble.score 'A'
    assert.are.equal 1, result

  it 'valuable letter', ->
    result = Scrabble.score 'f'
    assert.are.equal 4, result

  it 'short word', ->
    result = Scrabble.score 'at'
    assert.are.equal 2, result

  it 'short, valuable word', ->
    result = Scrabble.score 'zoo'
    assert.are.equal 12, result

  it 'medium word', ->
    result = Scrabble.score 'street'
    assert.are.equal 6, result

  it 'medium, valuable word', ->
    result = Scrabble.score 'quirky'
    assert.are.equal 22, result

  it 'long, mixed-case word', ->
    result = Scrabble.score 'OxyphenButazone'
    assert.are.equal 41, result

  it 'english-like word', ->
    result = Scrabble.score 'pinata'
    assert.are.equal 8, result

  it 'empty input', ->
    result = Scrabble.score ''
    assert.are.equal 0, result

  it 'entire alphabet available', ->
    result = Scrabble.score 'abcdefghijklmnopqrstuvwxyz'
    assert.are.equal 87, result
