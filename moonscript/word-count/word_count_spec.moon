import count_words from require 'word_count'

describe 'word-count', ->
  -- ----------------------------------------------------------
  same_kv = (state, arguments) ->
    actual = arguments[1]
    expected = arguments[2]
    return false if #expected != #actual
    for k, v in pairs expected
      return false if actual[k] != v
    true

  say = require 'say'
  say\set 'assertion.same_kv.positive', 'Actual result\n%s\ndoes not have the same keys and values as expected\n%s'
  say\set 'assertion.same_kv.negative', 'Actual result\n%s\nwas not supposed to be the same as the expected value.'
  assert\register 'assertion', 'same_kv', same_kv, 'assertion.same_kv.positive', 'assertion.same_kv.negative'
  -- ----------------------------------------------------------

  it 'count one word', ->
    result = count_words "word"
    expected = {
      word: 1,
    }
    assert.has.same_kv result, expected

  it 'count one of each word', ->
    result = count_words "one of each"
    expected = {
      one: 1,
      each: 1,
      of: 1,
    }
    assert.has.same_kv result, expected

  it 'multiple occurrences of a word', ->
    result = count_words "one fish two fish red fish blue fish"
    expected = {
      one: 1,
      fish: 4,
      blue: 1,
      red: 1,
      two: 1,
    }
    assert.has.same_kv result, expected

  it 'handles cramped lists', ->
    result = count_words "one,two,three"
    expected = {
      one: 1,
      two: 1,
      three: 1,
    }
    assert.has.same_kv result, expected

  it 'handles expanded lists', ->
    result = count_words "one,\ntwo,\nthree"
    expected = {
      one: 1,
      two: 1,
      three: 1,
    }
    assert.has.same_kv result, expected

  it 'ignore punctuation', ->
    result = count_words "car: carpet as java: javascript!!&@$%^&"
    expected = {
      carpet: 1,
      car: 1,
      javascript: 1,
      as: 1,
      java: 1,
    }
    assert.has.same_kv result, expected

  it 'include numbers', ->
    result = count_words "testing, 1, 2 testing"
    expected = {
      '1': 1,
      '2': 1,
      testing: 2,
    }
    assert.has.same_kv result, expected

  it 'normalize case', ->
    result = count_words "go Go GO Stop stop"
    expected = {
      stop: 2,
      go: 3,
    }
    assert.has.same_kv result, expected

  it 'with apostrophes', ->
    result = count_words "'First: don't laugh. Then: don't cry. You're getting it.'"
    expected = {
      then: 1,
      cry: 1,
      "you're": 1,
      getting: 1,
      "don't": 2,
      first: 1,
      laugh: 1,
      it: 1,
    }
    assert.has.same_kv result, expected

  it 'with quotations', ->
    result = count_words "Joe can't tell between 'large' and large."
    expected = {
      between: 1,
      tell: 1,
      joe: 1,
      large: 2,
      "can't": 1,
      and: 1,
    }
    assert.has.same_kv result, expected

  it 'substrings from the beginning', ->
    result = count_words "Joe can't tell between app, apple and a."
    expected = {
      between: 1,
      a: 1,
      tell: 1,
      app: 1,
      joe: 1,
      apple: 1,
      "can't": 1,
      and: 1,
    }
    assert.has.same_kv result, expected

  it 'multiple spaces not detected as a word', ->
    result = count_words " multiple   whitespaces"
    expected = {
      multiple: 1,
      whitespaces: 1,
    }
    assert.has.same_kv result, expected

  it 'alternating word separators not detected as a word', ->
    result = count_words ",\n,one,\n ,two \n 'three'"
    expected = {
      one: 1,
      two: 1,
      three: 1,
    }
    assert.has.same_kv result, expected

  it 'quotation for word with apostrophe', ->
    result = count_words "can, can't, 'can't'"
    expected = {
      "can't": 2,
      can: 1,
    }
    assert.has.same_kv result, expected
