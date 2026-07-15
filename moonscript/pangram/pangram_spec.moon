is_pangram = require 'pangram'

describe 'pangram:', ->
  it 'empty sentence', ->
    result = is_pangram ''
    assert.is_false result

  it 'perfect lower case', ->
    result = is_pangram 'abcdefghijklmnopqrstuvwxyz'
    assert.is_true result

  it 'only lower case', ->
    result = is_pangram 'the quick brown fox jumps over the lazy dog'
    assert.is_true result

  it "missing the letter 'x'", ->
    result = is_pangram 'a quick movement of the enemy will jeopardize five gunboats'
    assert.is_false result

  it "missing the letter 'h'", ->
    result = is_pangram 'five boxing wizards jump quickly at it'
    assert.is_false result

  it 'with underscores', ->
    result = is_pangram 'the_quick_brown_fox_jumps_over_the_lazy_dog'
    assert.is_true result

  it 'with numbers', ->
    result = is_pangram 'the 1 quick brown fox jumps over the 2 lazy dogs'
    assert.is_true result

  it 'missing letters replaced by numbers', ->
    result = is_pangram '7h3 qu1ck brown fox jumps ov3r 7h3 lazy dog'
    assert.is_false result

  it 'mixed case and punctuation', ->
    result = is_pangram '"Five quacking Zephyrs jolt my wax bed."'
    assert.is_true result

  it 'a-m and A-M are 26 different characters but not a pangram', ->
    result = is_pangram 'abcdefghijklm ABCDEFGHIJKLM'
    assert.is_false result

