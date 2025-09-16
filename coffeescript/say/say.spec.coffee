Say = require './say'

describe 'Say', ->
  it 'zero', ->
    expect(Say.say 0).toEqual 'zero'

  it 'one', ->
    expect(Say.say 1).toEqual 'one'

  it 'fourteen', ->
    expect(Say.say 14).toEqual 'fourteen'
  
  it 'twenty', ->
    expect(Say.say 20).toEqual 'twenty'

  it 'twenty-two', ->
    expect(Say.say 22).toEqual 'twenty-two'

  it 'thirty', ->
    expect(Say.say 30).toEqual 'thirty'

  it 'ninety-nine', ->
    expect(Say.say 99).toEqual 'ninety-nine'

  it 'one hundred', ->
    expect(Say.say 100).toEqual 'one hundred'

  it 'one hundred twenty-three', ->
    expect(Say.say 123).toEqual 'one hundred twenty-three'

  it 'two hundred', ->
    expect(Say.say 200).toEqual 'two hundred'

  it 'nine hundred ninety-nine', ->
    expect(Say.say 999).toEqual 'nine hundred ninety-nine'

  it 'one thousand', ->
    expect(Say.say 1000).toEqual 'one thousand'

  it 'one thousand two hundred thirty-four', ->
    expect(Say.say 1234).toEqual 'one thousand two hundred thirty-four'

  it 'one million', ->
    expect(Say.say 1000000).toEqual 'one million'

  it 'one million two thousand three hundred forty-five', ->
    expect(Say.say 1002345).toEqual 'one million two thousand three hundred forty-five'

  it 'one billion', ->
    expect(Say.say 1000000000).toEqual 'one billion'

  it 'a big number', ->
    expect(Say.say 987654321123).toEqual 'nine hundred eighty-seven billion ' + 
                                         'six hundred fifty-four million ' +
                                         'three hundred twenty-one thousand ' +
                                         'one hundred twenty-three'

  it 'numbers below zero are out of range', ->
    expect ->
      Say.say -1
    .toThrow new Error 'input out of range'

  it 'numbers above 999,999,999,999 are out of range', ->
    expect ->
      Say.say 1000000000000
    .toThrow new Error 'input out of range'
