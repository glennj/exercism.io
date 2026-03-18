SecretHandshake = require 'secret_handshake'

describe 'secret-handshake', ->
  it 'wink for 1', ->
    result = SecretHandshake.commands 1
    expected = {'wink'}
    assert.are.same expected, result

  it 'double blink for 10', ->
    result = SecretHandshake.commands 2
    expected = {'double blink'}
    assert.are.same expected, result

  it 'close your eyes for 100', ->
    result = SecretHandshake.commands 4
    expected = {'close your eyes'}
    assert.are.same expected, result

  it 'jump for 1000', ->
    result = SecretHandshake.commands 8
    expected = {'jump'}
    assert.are.same expected, result

  it 'combine two actions', ->
    result = SecretHandshake.commands 3
    expected = {'wink', 'double blink'}
    assert.are.same expected, result

  it 'reverse two actions', ->
    result = SecretHandshake.commands 19
    expected = {'double blink', 'wink'}
    assert.are.same expected, result

  it 'reversing one action gives the same action', ->
    result = SecretHandshake.commands 24
    expected = {'jump'}
    assert.are.same expected, result

  it 'reversing no actions still gives no actions', ->
    result = SecretHandshake.commands 16
    expected = {}
    assert.are.same expected, result

  it 'all possible actions', ->
    result = SecretHandshake.commands 15
    expected = {'wink', 'double blink', 'close your eyes', 'jump'}
    assert.are.same expected, result

  it 'reverse all possible actions', ->
    result = SecretHandshake.commands 31
    expected = {'jump', 'close your eyes', 'double blink', 'wink'}
    assert.are.same expected, result

  it 'do nothing for zero', ->
    result = SecretHandshake.commands 0
    expected = {}
    assert.are.same expected, result
