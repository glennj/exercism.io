RomanNumerals = require 'roman_numerals'

describe 'roman-numerals', ->
  it '1 is I', ->
    assert.are.equal 'I', RomanNumerals.to_roman 1

  it '2 is II', ->
    assert.are.equal 'II', RomanNumerals.to_roman 2

  it '3 is III', ->
    assert.are.equal 'III', RomanNumerals.to_roman 3

  it '4 is IV', ->
    assert.are.equal 'IV', RomanNumerals.to_roman 4

  it '5 is V', ->
    assert.are.equal 'V', RomanNumerals.to_roman 5

  it '6 is VI', ->
    assert.are.equal 'VI', RomanNumerals.to_roman 6

  it '9 is IX', ->
    assert.are.equal 'IX', RomanNumerals.to_roman 9

  it '16 is XVI', ->
    assert.are.equal 'XVI', RomanNumerals.to_roman 16

  it '27 is XXVII', ->
    assert.are.equal 'XXVII', RomanNumerals.to_roman 27

  it '48 is XLVIII', ->
    assert.are.equal 'XLVIII', RomanNumerals.to_roman 48

  it '49 is XLIX', ->
    assert.are.equal 'XLIX', RomanNumerals.to_roman 49

  it '59 is LIX', ->
    assert.are.equal 'LIX', RomanNumerals.to_roman 59

  it '66 is LXVI', ->
    assert.are.equal 'LXVI', RomanNumerals.to_roman 66

  it '93 is XCIII', ->
    assert.are.equal 'XCIII', RomanNumerals.to_roman 93

  it '141 is CXLI', ->
    assert.are.equal 'CXLI', RomanNumerals.to_roman 141

  it '163 is CLXIII', ->
    assert.are.equal 'CLXIII', RomanNumerals.to_roman 163

  it '166 is CLXVI', ->
    assert.are.equal 'CLXVI', RomanNumerals.to_roman 166

  it '402 is CDII', ->
    assert.are.equal 'CDII', RomanNumerals.to_roman 402

  it '575 is DLXXV', ->
    assert.are.equal 'DLXXV', RomanNumerals.to_roman 575

  it '666 is DCLXVI', ->
    assert.are.equal 'DCLXVI', RomanNumerals.to_roman 666

  it '911 is CMXI', ->
    assert.are.equal 'CMXI', RomanNumerals.to_roman 911

  it '1024 is MXXIV', ->
    assert.are.equal 'MXXIV', RomanNumerals.to_roman 1024

  it '1666 is MDCLXVI', ->
    assert.are.equal 'MDCLXVI', RomanNumerals.to_roman 1666

  it '3000 is MMM', ->
    assert.are.equal 'MMM', RomanNumerals.to_roman 3000

  it '3001 is MMMI', ->
    assert.are.equal 'MMMI', RomanNumerals.to_roman 3001

  it '3888 is MMMDCCCLXXXVIII', ->
    assert.are.equal 'MMMDCCCLXXXVIII', RomanNumerals.to_roman 3888

  it '3999 is MMMCMXCIX', ->
    assert.are.equal 'MMMCMXCIX', RomanNumerals.to_roman 3999
