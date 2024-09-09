Isogram = require './isogram'

describe 'Isogram', ->
  it 'empty string', ->
    phrase = ''
    result = Isogram.isIsogram(phrase)
    expect(result).toEqual true

  it 'isogram with only lower case characters', ->
    phrase = 'isogram'
    result = Isogram.isIsogram(phrase)
    expect(result).toEqual true

  it 'word with one duplicated character', ->
    phrase = 'eleven'
    result = Isogram.isIsogram(phrase)
    expect(result).toEqual false

  it 'word with one duplicated character from the end of the alphabet', ->
    phrase = 'zzyzx'
    result = Isogram.isIsogram(phrase)
    expect(result).toEqual false

  it 'longest reported english isogram', ->
    phrase = 'subdermatoglyphic'
    result = Isogram.isIsogram(phrase)
    expect(result).toEqual true

  it 'word with duplicated character in mixed case', ->
    phrase = 'Alphabet'
    result = Isogram.isIsogram(phrase)
    expect(result).toEqual false

  it 'word with duplicated character in mixed case, lowercase first', ->
    phrase = 'alphAbet'
    result = Isogram.isIsogram(phrase)
    expect(result).toEqual false

  it 'hypothetical isogrammic word with hyphen', ->
    phrase = 'thumbscrew-japingly'
    result = Isogram.isIsogram(phrase)
    expect(result).toEqual true

  it 'hypothetical word with duplicated character following hyphen', ->
    phrase = 'thumbscrew-jappingly'
    result = Isogram.isIsogram(phrase)
    expect(result).toEqual false

  it 'isogram with duplicated hyphen', ->
    phrase = 'six-year-old'
    result = Isogram.isIsogram(phrase)
    expect(result).toEqual true

  it 'made-up name that is an isogram', ->
    phrase = 'Emily Jung Schwartzkopf'
    result = Isogram.isIsogram(phrase)
    expect(result).toEqual true

  it 'duplicated character in the middle', ->
    phrase = 'accentor'
    result = Isogram.isIsogram(phrase)
    expect(result).toEqual false

  it 'same first and last characters', ->
    phrase = 'angola'
    result = Isogram.isIsogram(phrase)
    expect(result).toEqual false

  it 'word with duplicated character and with two hyphens', ->
    phrase = 'up-to-date'
    result = Isogram.isIsogram(phrase)
    expect(result).toEqual false
