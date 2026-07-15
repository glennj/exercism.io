SaveTheCow = require 'save_the_cow'

describe 'save-the-cow:', ->
  it 'Initially 9 failures are allowed and no letters are guessed', ->
    game = SaveTheCow 'loot'
    result = game\guess {}
    expected = {
      remainingFailures: 9
      state: 'Ongoing'
      maskedWord: '____'
    }
    assert.are.same expected, result

  it 'After 10 failures the game is over', ->
    game = SaveTheCow 'loot'
    result = game\guess {'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j'}
    expected = {
      remainingFailures: 0
      state: 'Lose'
      maskedWord: '____'
    }
    assert.are.same expected, result

  it 'Losing with several correct guesses', ->
    game = SaveTheCow 'loot'
    result = game\guess {'t', 'o', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j'}
    expected = {
      remainingFailures: 0
      state: 'Lose'
      maskedWord: '_oot'
    }
    assert.are.same expected, result

  it 'Feeding a correct letter removes underscores', ->
    game = SaveTheCow 'loot'
    result = game\guess {'t'}
    expected = {
      remainingFailures: 9
      state: 'Ongoing'
      maskedWord: '___t'
    }
    assert.are.same expected, result

  it 'Feeding a correct letter twice counts as a failure', ->
    game = SaveTheCow 'loot'
    result = game\guess {'t', 't'}
    expected = {
      remainingFailures: 8
      state: 'Ongoing'
      maskedWord: '___t'
    }
    assert.are.same expected, result

  it 'Guessing a repeated letter reveals all instances', ->
    game = SaveTheCow 'loot'
    result = game\guess {'t', 't', 'o'}
    expected = {
      remainingFailures: 8
      state: 'Ongoing'
      maskedWord: '_oot'
    }
    assert.are.same expected, result

  it 'Getting all the letters right makes for a win', ->
    game = SaveTheCow 'loot'
    result = game\guess {'t', 't', 'o', 'l'}
    expected = {
      remainingFailures: 8
      state: 'Win'
      maskedWord: 'loot'
    }
    assert.are.same expected, result

  it 'Winning on the last guess is still a win', ->
    game = SaveTheCow 'loot'
    result = game\guess {'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 't', 'o', 'l'}
    expected = {
      remainingFailures: 0
      state: 'Win'
      maskedWord: 'loot'
    }
    assert.are.same expected, result

  it 'Guessing after a lose is error', ->
    game = SaveTheCow 'loot'
    f = -> game\guess {'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'}
    assert.has.error f, 'cannot guess after the game is lost'

  it 'Guessing after a win is error', ->
    game = SaveTheCow 'loot'
    f = -> game\guess {'t', 'o', 'l', 'l'}
    assert.has.error f, 'cannot guess after the game is won'

