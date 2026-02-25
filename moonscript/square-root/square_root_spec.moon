SquareRoot = require 'square_root'

describe 'square-root', ->
  it 'root of 1', ->
    result = SquareRoot.sqrt 1
    assert.are.equal 1, result

  it 'root of 4', ->
    result = SquareRoot.sqrt 4
    assert.are.equal 2, result

  it 'root of 25', ->
    result = SquareRoot.sqrt 25
    assert.are.equal 5, result

  it 'root of 81', ->
    result = SquareRoot.sqrt 81
    assert.are.equal 9, result

  it 'root of 196', ->
    result = SquareRoot.sqrt 196
    assert.are.equal 14, result

  it 'root of 65025', ->
    result = SquareRoot.sqrt 65025
    assert.are.equal 255, result
