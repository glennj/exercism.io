Luhn = require 'luhn'

describe 'luhn', ->
  it 'single digit strings can not be valid', ->
    assert.is_false Luhn.is_valid '1'

  it 'a single zero is invalid', ->
    assert.is_false Luhn.is_valid '0'

  it 'a simple valid SIN that remains valid if reversed', ->
    assert.is_true Luhn.is_valid '059'

  it 'a simple valid SIN that becomes invalid if reversed', ->
    assert.is_true Luhn.is_valid '59'

  it 'a valid Canadian SIN', ->
    assert.is_true Luhn.is_valid '055 444 285'

  it 'invalid Canadian SIN', ->
    assert.is_false Luhn.is_valid '055 444 286'

  it 'invalid credit card', ->
    assert.is_false Luhn.is_valid '8273 1232 7352 0569'

  it 'invalid long number with an even remainder', ->
    assert.is_false Luhn.is_valid '1 2345 6789 1234 5678 9012'

  it 'invalid long number with a remainder divisible by 5', ->
    assert.is_false Luhn.is_valid '1 2345 6789 1234 5678 9013'

  it 'valid number with an even number of digits', ->
    assert.is_true Luhn.is_valid '095 245 88'

  it 'valid number with an odd number of spaces', ->
    assert.is_true Luhn.is_valid '234 567 891 234'

  it 'valid strings with a non-digit added at the end become invalid', ->
    assert.is_false Luhn.is_valid '059a'

  it 'valid strings with punctuation included become invalid', ->
    assert.is_false Luhn.is_valid '055-444-285'

  it 'valid strings with symbols included become invalid', ->
    assert.is_false Luhn.is_valid '055# 444$ 285'

  it 'single zero with space is invalid', ->
    assert.is_false Luhn.is_valid ' 0'

  it 'more than a single zero is valid', ->
    assert.is_true Luhn.is_valid '0000 0'

  it 'input digit 9 is correctly converted to output digit 9', ->
    assert.is_true Luhn.is_valid '091'

  it 'very long input is valid', ->
    assert.is_true Luhn.is_valid '9999999999 9999999999 9999999999 9999999999'

  it 'valid luhn with an odd number of digits and non zero first digit', ->
    assert.is_true Luhn.is_valid '109'

  it "using ascii value for non-doubled non-digit isn't allowed", ->
    assert.is_false Luhn.is_valid '055b 444 285'

  it "using ascii value for doubled non-digit isn't allowed", ->
    assert.is_false Luhn.is_valid ':9'

  it "non-numeric, non-space char in the middle with a sum that's divisible by 10 isn't allowed", ->
    assert.is_false Luhn.is_valid '59%59'
