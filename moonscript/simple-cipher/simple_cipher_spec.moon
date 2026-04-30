SimpleCipher = require 'simple_cipher'

describe 'simple-cipher', ->
  describe 'Random key cipher', ->
    it 'Can encode', ->
      cipher = SimpleCipher!
      plaintext = 'aaaaaaaaaa'
      encoded = cipher\encode plaintext
      expected = cipher\key!\sub(1, #plaintext)
      assert.are.equal expected, encoded

    it 'Can decode', ->
      cipher = SimpleCipher!
      expected = 'aaaaaaaaaa'
      ciphertext = cipher\key!\sub(1, #expected)
      decoded = cipher\decode ciphertext
      assert.are.equal expected, decoded

    it 'Is reversible. I.e., if you apply decode in a encoded result, you must see the same plaintext encode parameter as a result of the decode method', ->
      cipher = SimpleCipher!
      plaintext = 'abcdefghij'
      ciphertext = cipher\encode plaintext
      decoded = cipher\decode ciphertext
      assert.are.equal plaintext, decoded

    it 'Key is made only of lowercase letters', ->
      cipher = SimpleCipher!
      key = cipher\key!
      assert.is.truthy key\match('^[a-z]+$')

  describe 'Substitution cipher', ->
    it 'Can encode', ->
      cipher = SimpleCipher 'abcdefghij'
      result = cipher\encode 'aaaaaaaaaa'
      assert.are.equal 'abcdefghij', result

    it 'Can decode', ->
      cipher = SimpleCipher 'abcdefghij'
      result = cipher\decode 'abcdefghij'
      assert.are.equal 'aaaaaaaaaa', result

    it 'Is reversible. I.e., if you apply decode in a encoded result, you must see the same plaintext encode parameter as a result of the decode method', ->
      cipher = SimpleCipher 'abcdefghij'
      ciphertext = cipher\encode 'abcdefghij'
      result = cipher\decode ciphertext
      assert.are.equal 'abcdefghij', result

    it 'Can double shift encode', ->
      cipher = SimpleCipher 'iamapandabear'
      result = cipher\encode 'iamapandabear'
      assert.are.equal 'qayaeaagaciai', result

    it 'Can wrap on encode', ->
      cipher = SimpleCipher 'abcdefghij'
      result = cipher\encode 'zzzzzzzzzz'
      assert.are.equal 'zabcdefghi', result

    it 'Can wrap on decode', ->
      cipher = SimpleCipher 'abcdefghij'
      result = cipher\decode 'zabcdefghi'
      assert.are.equal 'zzzzzzzzzz', result

    it 'Can encode messages longer than the key', ->
      cipher = SimpleCipher 'abc'
      result = cipher\encode 'iamapandabear'
      assert.are.equal 'iboaqcnecbfcr', result

    it 'Can decode messages longer than the key', ->
      cipher = SimpleCipher 'abc'
      result = cipher\decode 'iboaqcnecbfcr'
      assert.are.equal 'iamapandabear', result
