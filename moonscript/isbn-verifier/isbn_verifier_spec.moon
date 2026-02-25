Isbn10 = require 'isbn_verifier'

describe 'isbn-verifier', ->
  it 'valid isbn', ->
    assert.is_true Isbn10.is_valid '3-598-21508-8'

  it 'invalid isbn check digit', ->
    assert.is_false Isbn10.is_valid '3-598-21508-9'

  it 'valid isbn with a check digit of 10', ->
    assert.is_true Isbn10.is_valid '3-598-21507-X'

  it 'check digit is a character other than X', ->
    assert.is_false Isbn10.is_valid '3-598-21507-A'

  it 'invalid check digit in isbn is not treated as zero', ->
    assert.is_false Isbn10.is_valid '4-598-21507-B'

  it 'invalid character in isbn is not treated as zero', ->
    assert.is_false Isbn10.is_valid '3-598-P1581-X'

  it 'X is only valid as a check digit', ->
    assert.is_false Isbn10.is_valid '3-598-2X507-9'

  it 'only one check digit is allowed', ->
    assert.is_false Isbn10.is_valid '3-598-21508-96'

  it 'X is not substituted by the value 10', ->
    assert.is_false Isbn10.is_valid '3-598-2X507-5'

  it 'valid isbn without separating dashes', ->
    assert.is_true Isbn10.is_valid '3598215088'

  it 'isbn without separating dashes and X as check digit', ->
    assert.is_true Isbn10.is_valid '359821507X'

  it 'isbn without check digit and dashes', ->
    assert.is_false Isbn10.is_valid '359821507'

  it 'too long isbn and no dashes', ->
    assert.is_false Isbn10.is_valid '3598215078X'

  it 'too short isbn', ->
    assert.is_false Isbn10.is_valid '00'

  it 'isbn without check digit', ->
    assert.is_false Isbn10.is_valid '3-598-21507'

  it 'check digit of X should not be used for 0', ->
    assert.is_false Isbn10.is_valid '3-598-21515-X'

  it 'empty isbn', ->
    assert.is_false Isbn10.is_valid ''

  it 'input is 9 characters', ->
    assert.is_false Isbn10.is_valid '134456729'

  it 'invalid characters are not ignored after checking length', ->
    assert.is_false Isbn10.is_valid '3132P34035'

  it 'invalid characters are not ignored before checking length', ->
    assert.is_false Isbn10.is_valid '3598P215088'

  it 'input is too long but contains a valid isbn', ->
    assert.is_false Isbn10.is_valid '98245726788'
