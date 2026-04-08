import clean from require 'phone_number'

describe 'phone-number', ->
  it 'cleans the number', ->
    result = clean '(223) 456-7890'
    assert.is.equal '2234567890', result

  it 'cleans numbers with dots', ->
    result = clean '223.456.7890'
    assert.is.equal '2234567890', result

  it 'cleans numbers with multiple spaces', ->
    result = clean '223 456   7890   '
    assert.is.equal '2234567890', result

  it 'invalid when 9 digits', ->
    f = -> clean '123456789'
    assert.has.error f, 'must not be fewer than 10 digits'

  it 'invalid when 11 digits does not start with a 1', ->
    f = -> clean '22234567890'
    assert.has.error f, '11 digits must start with 1'

  it 'valid when 11 digits and starting with 1', ->
    result = clean '12234567890'
    assert.is.equal '2234567890', result

  it 'valid when 11 digits and starting with 1 even with punctuation', ->
    result = clean '+1 (223) 456-7890'
    assert.is.equal '2234567890', result

  it 'invalid when more than 11 digits', ->
    f = -> clean '321234567890'
    assert.has.error f, 'must not be greater than 11 digits'

  it 'invalid with letters', ->
    f = -> clean '523-abc-7890'
    assert.has.error f, 'letters not permitted'

  it 'invalid with punctuations', ->
    f = -> clean '523-@:!-7890'
    assert.has.error f, 'punctuations not permitted'

  it 'invalid if area code starts with 0', ->
    f = -> clean '(023) 456-7890'
    assert.has.error f, 'area code cannot start with zero'

  it 'invalid if area code starts with 1', ->
    f = -> clean '(123) 456-7890'
    assert.has.error f, 'area code cannot start with one'

  it 'invalid if exchange code starts with 0', ->
    f = -> clean '(223) 056-7890'
    assert.has.error f, 'exchange code cannot start with zero'

  it 'invalid if exchange code starts with 1', ->
    f = -> clean '(223) 156-7890'
    assert.has.error f, 'exchange code cannot start with one'

  it 'invalid if area code starts with 0 on valid 11-digit number', ->
    f = -> clean '1 (023) 456-7890'
    assert.has.error f, 'area code cannot start with zero'

  it 'invalid if area code starts with 1 on valid 11-digit number', ->
    f = -> clean '1 (123) 456-7890'
    assert.has.error f, 'area code cannot start with one'

  it 'invalid if exchange code starts with 0 on valid 11-digit number', ->
    f = -> clean '1 (223) 056-7890'
    assert.has.error f, 'exchange code cannot start with zero'

  it 'invalid if exchange code starts with 1 on valid 11-digit number', ->
    f = -> clean '1 (223) 156-7890'
    assert.has.error f, 'exchange code cannot start with one'
