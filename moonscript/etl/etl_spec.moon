Etl = require 'etl'

describe 'etl', ->
  it 'single letter', ->
    legacy = {
      '1': {'A'}
    }
    expected = {
      'a': 1
    }
    result = Etl.transform legacy
    assert.are.same expected, result

  it 'single score with multiple letters', ->
    legacy = {
      '1': {'A', 'E', 'I', 'O', 'U'}
    }
    expected = {
      'a': 1
      'e': 1
      'u': 1
      'o': 1
      'i': 1
    }
    result = Etl.transform legacy
    assert.are.same expected, result

  it 'multiple scores with multiple letters', ->
    legacy = {
      '1': {'A', 'E'}
      '2': {'D', 'G'}
    }
    expected = {
      'a': 1
      'd': 2
      'g': 2
      'e': 1
    }
    result = Etl.transform legacy
    assert.are.same expected, result

  it 'multiple scores with differing numbers of letters', ->
    legacy = {
      '1': {'A', 'E', 'I', 'O', 'U', 'L', 'N', 'R', 'S', 'T'}
      '8': {'J', 'X'}
      '3': {'B', 'C', 'M', 'P'}
      '2': {'D', 'G'}
      '5': {'K'}
      '4': {'F', 'H', 'V', 'W', 'Y'}
      '10': {'Q', 'Z'}
    }
    expected = {
      'a': 1
      'c': 3
      'b': 3
      'e': 1
      'd': 2
      'g': 2
      'f': 4
      'i': 1
      'h': 4
      'k': 5
      'j': 8
      'm': 3
      'l': 1
      'o': 1
      'n': 1
      'q': 10
      'p': 3
      's': 1
      'r': 1
      'u': 1
      't': 1
      'w': 4
      'v': 4
      'y': 4
      'x': 8
      'z': 10
    }
    result = Etl.transform legacy
    assert.are.same expected, result
