import encode, decode from require 'affine_cipher'

describe 'affine-cipher', ->
  describe 'encode', ->
    it 'encode yes', ->
      result = encode 'yes', {a: 5, b: 7}
      expected = 'xbt'
      assert.are.equal expected, result

    pending 'encode no', ->
      result = encode 'no', {a: 15, b: 18}
      expected = 'fu'
      assert.are.equal expected, result

    pending 'encode OMG', ->
      result = encode 'OMG', {a: 21, b: 3}
      expected = 'lvz'
      assert.are.equal expected, result

    pending 'encode O M G', ->
      result = encode 'O M G', {a: 25, b: 47}
      expected = 'hjp'
      assert.are.equal expected, result

    pending 'encode mindblowingly', ->
      result = encode 'mindblowingly', {a: 11, b: 15}
      expected = 'rzcwa gnxzc dgt'
      assert.are.equal expected, result

    pending 'encode numbers', ->
      result = encode 'Testing,1 2 3, testing.', {a: 3, b: 4}
      expected = 'jqgjc rw123 jqgjc rw'
      assert.are.equal expected, result

    pending 'encode deep thought', ->
      result = encode 'Truth is fiction.', {a: 5, b: 17}
      expected = 'iynia fdqfb ifje'
      assert.are.equal expected, result

    pending 'encode all the letters', ->
      result = encode 'The quick brown fox jumps over the lazy dog.', {a: 17, b: 33}
      expected = 'swxtj npvyk lruol iejdc blaxk swxmh qzglf'
      assert.are.equal expected, result

    pending 'encode with a not coprime to m', ->
      f = -> encode 'This is a test.', {a: 6, b: 17}
      assert.has.error f, 'a and m must be coprime.'

  describe 'decode', ->
    pending 'decode exercism', ->
      result = decode 'tytgn fjr', {a: 3, b: 7}
      expected = 'exercism'
      assert.are.equal expected, result

    pending 'decode a sentence', ->
      result = decode 'qdwju nqcro muwhn odqun oppmd aunwd o', {a: 19, b: 16}
      expected = 'anobstacleisoftenasteppingstone'
      assert.are.equal expected, result

    pending 'decode numbers', ->
      result = decode 'odpoz ub123 odpoz ub', {a: 25, b: 7}
      expected = 'testing123testing'
      assert.are.equal expected, result

    pending 'decode all the letters', ->
      result = decode 'swxtj npvyk lruol iejdc blaxk swxmh qzglf', {a: 17, b: 33}
      expected = 'thequickbrownfoxjumpsoverthelazydog'
      assert.are.equal expected, result

    pending 'decode with no spaces in input', ->
      result = decode 'swxtjnpvyklruoliejdcblaxkswxmhqzglf', {a: 17, b: 33}
      expected = 'thequickbrownfoxjumpsoverthelazydog'
      assert.are.equal expected, result

    pending 'decode with too many spaces', ->
      result = decode 'vszzm    cly   yd cg    qdp', {a: 15, b: 16}
      expected = 'jollygreengiant'
      assert.are.equal expected, result

    pending 'decode with a not coprime to m', ->
      f = -> decode 'Test', {a: 13, b: 5}
      assert.has.error f, 'a and m must be coprime.'
