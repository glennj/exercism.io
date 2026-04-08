import encode from require 'crypto_square'

describe 'crypto-square', ->
  it 'empty plaintext results in an empty ciphertext', ->
    result = encode ''
    expected = ''
    assert.are.equal expected, result

  it 'normalization results in empty plaintext', ->
    result = encode '... --- ...'
    expected = ''
    assert.are.equal expected, result

  it 'Lowercase', ->
    result = encode 'A'
    expected = 'a'
    assert.are.equal expected, result

  it 'Remove spaces', ->
    result = encode '  b '
    expected = 'b'
    assert.are.equal expected, result

  it 'Remove punctuation', ->
    result = encode '@1,%!'
    expected = '1'
    assert.are.equal expected, result

  it '9 character plaintext results in 3 chunks of 3 characters', ->
    result = encode 'This is fun!'
    expected = 'tsf hiu isn'
    assert.are.equal expected, result

  it '8 character plaintext results in 3 chunks, the last one with a trailing space', ->
    result = encode 'Chill out.'
    expected = 'clu hlt io '
    assert.are.equal expected, result

  it '54 character plaintext results in 8 chunks, the last two with trailing spaces', ->
    result = encode 'If man was meant to stay on the ground, god would have given us roots.'
    expected = 'imtgdvs fearwer mayoogo anouuio ntnnlvt wttddes aohghn  sseoau '
    assert.are.equal expected, result
