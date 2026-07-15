import is_isogram from require 'isogram'

describe 'isogram:', ->
  it 'empty string', ->
    assert.is_true is_isogram ''

  it 'isogram with only lower case characters', ->
    assert.is_true is_isogram 'isogram'

  it 'word with one duplicated character', ->
    assert.is_false is_isogram 'eleven'

  it 'word with one duplicated character from the end of the alphabet', ->
    assert.is_false is_isogram 'zzyzx'

  it 'longest reported english isogram', ->
    assert.is_true is_isogram 'subdermatoglyphic'

  it 'word with duplicated character in mixed case', ->
    assert.is_false is_isogram 'Alphabet'

  it 'word with duplicated character in mixed case, lowercase first', ->
    assert.is_false is_isogram 'alphAbet'

  it 'hypothetical isogrammic word with hyphen', ->
    assert.is_true is_isogram 'thumbscrew-japingly'

  it 'hypothetical word with duplicated character following hyphen', ->
    assert.is_false is_isogram 'thumbscrew-jappingly'

  it 'isogram with duplicated hyphen', ->
    assert.is_true is_isogram 'six-year-old'

  it 'made-up name that is an isogram', ->
    assert.is_true is_isogram 'Emily Jung Schwartzkopf'

  it 'duplicated character in the middle', ->
    assert.is_false is_isogram 'accentor'

  it 'same first and last characters', ->
    assert.is_false is_isogram 'angola'

  it 'word with duplicated character and with two hyphens', ->
    assert.is_false is_isogram 'up-to-date'

