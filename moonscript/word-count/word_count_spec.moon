import count_words from require 'word_count'

describe 'word-count:', ->
  it 'count one word', ->
    result = count_words "word"
    expected = {
      word: 1,
    }
    assert.are.same result, expected

  it 'count one of each word', ->
    result = count_words "one of each"
    expected = {
      each: 1,
      of: 1,
      one: 1,
    }
    assert.are.same result, expected

  it 'multiple occurrences of a word', ->
    result = count_words "one fish two fish red fish blue fish"
    expected = {
      blue: 1,
      fish: 4,
      one: 1,
      red: 1,
      two: 1,
    }
    assert.are.same result, expected

  it 'handles cramped lists', ->
    result = count_words "one,two,three"
    expected = {
      one: 1,
      three: 1,
      two: 1,
    }
    assert.are.same result, expected

  it 'handles expanded lists', ->
    result = count_words "one,\ntwo,\nthree"
    expected = {
      one: 1,
      three: 1,
      two: 1,
    }
    assert.are.same result, expected

  it 'ignore punctuation', ->
    result = count_words "car: carpet as java: javascript!!&@$%^&"
    expected = {
      as: 1,
      car: 1,
      carpet: 1,
      java: 1,
      javascript: 1,
    }
    assert.are.same result, expected

  it 'include numbers', ->
    result = count_words "testing, 1, 2 testing"
    expected = {
      '1': 1,
      '2': 1,
      testing: 2,
    }
    assert.are.same result, expected

  it 'normalize case', ->
    result = count_words "go Go GO Stop stop"
    expected = {
      go: 3,
      stop: 2,
    }
    assert.are.same result, expected

  it 'with apostrophes', ->
    result = count_words "'First: don't laugh. Then: don't cry. You're getting it.'"
    expected = {
      cry: 1,
      "don't": 2,
      first: 1,
      getting: 1,
      it: 1,
      laugh: 1,
      then: 1,
      "you're": 1,
    }
    assert.are.same result, expected

  it 'with quotations', ->
    result = count_words "Joe can't tell between 'large' and large."
    expected = {
      and: 1,
      between: 1,
      "can't": 1,
      joe: 1,
      large: 2,
      tell: 1,
    }
    assert.are.same result, expected

  it 'substrings from the beginning', ->
    result = count_words "Joe can't tell between app, apple and a."
    expected = {
      a: 1,
      and: 1,
      app: 1,
      apple: 1,
      between: 1,
      "can't": 1,
      joe: 1,
      tell: 1,
    }
    assert.are.same result, expected

  it 'multiple spaces not detected as a word', ->
    result = count_words " multiple   whitespaces"
    expected = {
      multiple: 1,
      whitespaces: 1,
    }
    assert.are.same result, expected

  it 'alternating word separators not detected as a word', ->
    result = count_words ",\n,one,\n ,two \n 'three'"
    expected = {
      one: 1,
      three: 1,
      two: 1,
    }
    assert.are.same result, expected

  it 'quotation for word with apostrophe', ->
    result = count_words "can, can't, 'can't'"
    expected = {
      can: 1,
      "can't": 2,
    }
    assert.are.same result, expected

