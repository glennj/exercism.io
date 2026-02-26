Say = require './say'

describe 'say', ->
  it 'zero', ->
    result = Say.in_english 0
    expected = 'zero'
    assert.are.equal expected, result

  it 'one', ->
    result = Say.in_english 1
    expected = 'one'
    assert.are.equal expected, result

  it 'fourteen', ->
    result = Say.in_english 14
    expected = 'fourteen'
    assert.are.equal expected, result

  it 'twenty', ->
    result = Say.in_english 20
    expected = 'twenty'
    assert.are.equal expected, result

  it 'twenty-two', ->
    result = Say.in_english 22
    expected = 'twenty-two'
    assert.are.equal expected, result

  it 'thirty', ->
    result = Say.in_english 30
    expected = 'thirty'
    assert.are.equal expected, result

  it 'ninety-nine', ->
    result = Say.in_english 99
    expected = 'ninety-nine'
    assert.are.equal expected, result

  it 'one hundred', ->
    result = Say.in_english 100
    expected = 'one hundred'
    assert.are.equal expected, result

  it 'one hundred twenty-three', ->
    result = Say.in_english 123
    expected = 'one hundred twenty-three'
    assert.are.equal expected, result

  it 'two hundred', ->
    result = Say.in_english 200
    expected = 'two hundred'
    assert.are.equal expected, result

  it 'nine hundred ninety-nine', ->
    result = Say.in_english 999
    expected = 'nine hundred ninety-nine'
    assert.are.equal expected, result

  it 'one thousand', ->
    result = Say.in_english 1000
    expected = 'one thousand'
    assert.are.equal expected, result

  it 'one thousand two hundred thirty-four', ->
    result = Say.in_english 1234
    expected = 'one thousand two hundred thirty-four'
    assert.are.equal expected, result

  it 'one million', ->
    result = Say.in_english 1000000
    expected = 'one million'
    assert.are.equal expected, result

  it 'one million two thousand three hundred forty-five', ->
    result = Say.in_english 1002345
    expected = 'one million two thousand three hundred forty-five'
    assert.are.equal expected, result

  it 'one billion', ->
    result = Say.in_english 1000000000
    expected = 'one billion'
    assert.are.equal expected, result

  it 'a big number', ->
    result = Say.in_english 987654321123
    expected = 'nine hundred eighty-seven billion six hundred fifty-four million three hundred twenty-one thousand one hundred twenty-three'
    assert.are.equal expected, result

  it 'numbers below zero are out of range', ->
    f = -> Say.in_english -1
    assert.has.error f,'input out of range'

  it 'numbers above 999,999,999,999 are out of range', ->
    f = -> Say.in_english 1000000000000
    assert.has.error f,'input out of range'
