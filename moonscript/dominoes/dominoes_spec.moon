import canChain from require 'dominoes'

describe 'dominoes', ->
  it 'empty input = empty output', ->
    dominoes = {}
    assert.is.true canChain dominoes

  it 'singleton input = singleton output', ->
    dominoes = {{1, 1}}
    assert.is.true canChain dominoes

  it "singleton that can't be chained", ->
    dominoes = {{1, 2}}
    assert.is.false canChain dominoes

  it 'three elements', ->
    dominoes = {
      {1, 2},
      {3, 1},
      {2, 3},
    }
    assert.is.true canChain dominoes

  it 'can reverse dominoes', ->
    dominoes = {
      {1, 2},
      {1, 3},
      {2, 3},
    }
    assert.is.true canChain dominoes

  it "can't be chained", ->
    dominoes = {
      {1, 2},
      {4, 1},
      {2, 3},
    }
    assert.is.false canChain dominoes

  it 'disconnected - simple', ->
    dominoes = {
      {1, 1},
      {2, 2},
    }
    assert.is.false canChain dominoes

  it 'disconnected - double loop', ->
    dominoes = {
      {1, 2},
      {2, 1},
      {3, 4},
      {4, 3},
    }
    assert.is.false canChain dominoes

  it 'disconnected - single isolated', ->
    dominoes = {
      {1, 2},
      {2, 3},
      {3, 1},
      {4, 4},
    }
    assert.is.false canChain dominoes

  it 'need backtrack', ->
    dominoes = {
      {1, 2},
      {2, 3},
      {3, 1},
      {2, 4},
      {2, 4},
    }
    assert.is.true canChain dominoes

  it 'separate loops', ->
    dominoes = {
      {1, 2},
      {2, 3},
      {3, 1},
      {1, 1},
      {2, 2},
      {3, 3},
    }
    assert.is.true canChain dominoes

  it 'nine elements', ->
    dominoes = {
      {1, 2},
      {5, 3},
      {3, 1},
      {1, 2},
      {2, 4},
      {1, 6},
      {2, 3},
      {3, 4},
      {5, 6},
    }
    assert.is.true canChain dominoes

  it 'separate three-domino loops', ->
    dominoes = {
      {1, 2},
      {2, 3},
      {3, 1},
      {4, 5},
      {5, 6},
      {6, 4},
    }
    assert.is.false canChain dominoes
