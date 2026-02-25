reverse = require 'reverse_string'

describe 'reverse-string', ->
  it 'an empty string', ->
    result = reverse ""
    assert.are.same "", result

  it 'a word', ->
    result = reverse "robot"
    assert.are.same "tobor", result

  it 'a capitalized word', ->
    result = reverse "Ramen"
    assert.are.same "nemaR", result

  it 'a sentence with punctuation', ->
    result = reverse "I'm hungry!"
    assert.are.same "!yrgnuh m'I", result

  it 'a palindrome', ->
    result = reverse "racecar"
    assert.are.same "racecar", result

  it 'an even-sized word', ->
    result = reverse "drawer"
    assert.are.same "reward", result

  it 'wide characters', ->
    result = reverse "子猫"
    assert.are.same "猫子", result
