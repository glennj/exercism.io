import is_isogram from require 'isogram'

describe 'isogram', ->
  it 'empty string', ->
    assert.is_true is_isogram ''

  pending 'isogram with only lower case characters', ->
    assert.is_true is_isogram 'isogram'

  pending 'word with one duplicated character', ->
    assert.is_false is_isogram 'eleven'

  pending 'word with one duplicated character from the end of the alphabet', ->
    assert.is_false is_isogram 'zzyzx'

  pending 'longest reported english isogram', ->
    assert.is_true is_isogram 'subdermatoglyphic'

  pending 'word with duplicated character in mixed case', ->
    assert.is_false is_isogram 'Alphabet'

  pending 'word with duplicated character in mixed case, lowercase first', ->
    assert.is_false is_isogram 'alphAbet'

  pending 'hypothetical isogrammic word with hyphen', ->
    assert.is_true is_isogram 'thumbscrew-japingly'

  pending 'hypothetical word with duplicated character following hyphen', ->
    assert.is_false is_isogram 'thumbscrew-jappingly'

  pending 'isogram with duplicated hyphen', ->
    assert.is_true is_isogram 'six-year-old'

  pending 'made-up name that is an isogram', ->
    assert.is_true is_isogram 'Emily Jung Schwartzkopf'

  pending 'duplicated character in the middle', ->
    assert.is_false is_isogram 'accentor'

  pending 'same first and last characters', ->
    assert.is_false is_isogram 'angola'

  pending 'word with duplicated character and with two hyphens', ->
    assert.is_false is_isogram 'up-to-date'
