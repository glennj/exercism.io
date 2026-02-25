import encode, decode from require 'run_length_encoding'

describe 'run-length-encoding', ->
  describe 'run-length encode a string', ->
    it 'empty string', ->
      result = encode ''
      assert.are.equal '', result

    it 'single characters only are encoded without count', ->
      result = encode 'XYZ'
      assert.are.equal 'XYZ', result

    it 'string with no single characters', ->
      result = encode 'AABBBCCCC'
      assert.are.equal '2A3B4C', result

    it 'single characters mixed with repeated characters', ->
      result = encode 'WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB'
      assert.are.equal '12WB12W3B24WB', result

    it 'multiple whitespace mixed in string', ->
      result = encode '  hsqq qww  '
      assert.are.equal '2 hs2q q2w2 ', result

    it 'lowercase characters', ->
      result = encode 'aabbbcccc'
      assert.are.equal '2a3b4c', result

  describe 'run-length decode a string', ->
    it 'empty string', ->
      result = decode ''
      assert.are.equal '', result

    it 'single characters only', ->
      result = decode 'XYZ'
      assert.are.equal 'XYZ', result

    it 'string with no single characters', ->
      result = decode '2A3B4C'
      assert.are.equal 'AABBBCCCC', result

    it 'single characters with repeated characters', ->
      result = decode '12WB12W3B24WB'
      assert.are.equal 'WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB', result

    it 'multiple whitespace mixed in string', ->
      result = decode '2 hs2q q2w2 '
      assert.are.equal '  hsqq qww  ', result

    it 'lowercase string', ->
      result = decode '2a3b4c'
      assert.are.equal 'aabbbcccc', result

  describe 'encode and then decode', ->
    it 'encode followed by decode gives original string', ->
      encoded = encode 'zzz ZZ  zZ'
      decoded = decode encoded
      assert.are.equal 'zzz ZZ  zZ', decoded
