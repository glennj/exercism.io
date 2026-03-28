AtbashCipher = require 'atbash_cipher'

describe 'atbash-cipher', ->
  describe 'encode', ->
    it 'encode yes', ->
      result = AtbashCipher.encode 'yes'
      expected = 'bvh'
      assert.are.equal expected, result

    pending 'encode no', ->
      result = AtbashCipher.encode 'no'
      expected = 'ml'
      assert.are.equal expected, result

    pending 'encode OMG', ->
      result = AtbashCipher.encode 'OMG'
      expected = 'lnt'
      assert.are.equal expected, result

    pending 'encode spaces', ->
      result = AtbashCipher.encode 'O M G'
      expected = 'lnt'
      assert.are.equal expected, result

    pending 'encode mindblowingly', ->
      result = AtbashCipher.encode 'mindblowingly'
      expected = 'nrmwy oldrm tob'
      assert.are.equal expected, result

    pending 'encode numbers', ->
      result = AtbashCipher.encode 'Testing,1 2 3, testing.'
      expected = 'gvhgr mt123 gvhgr mt'
      assert.are.equal expected, result

    pending 'encode deep thought', ->
      result = AtbashCipher.encode 'Truth is fiction.'
      expected = 'gifgs rhurx grlm'
      assert.are.equal expected, result

    pending 'encode all the letters', ->
      result = AtbashCipher.encode 'The quick brown fox jumps over the lazy dog.'
      expected = 'gsvjf rxpyi ldmul cqfnk hlevi gsvoz abwlt'
      assert.are.equal expected, result

  describe 'decode', ->
    pending 'decode exercism', ->
      result = AtbashCipher.decode 'vcvix rhn'
      expected = 'exercism'
      assert.are.equal expected, result

    pending 'decode a sentence', ->
      result = AtbashCipher.decode 'zmlyh gzxov rhlug vmzhg vkkrm thglm v'
      expected = 'anobstacleisoftenasteppingstone'
      assert.are.equal expected, result

    pending 'decode numbers', ->
      result = AtbashCipher.decode 'gvhgr mt123 gvhgr mt'
      expected = 'testing123testing'
      assert.are.equal expected, result

    pending 'decode all the letters', ->
      result = AtbashCipher.decode 'gsvjf rxpyi ldmul cqfnk hlevi gsvoz abwlt'
      expected = 'thequickbrownfoxjumpsoverthelazydog'
      assert.are.equal expected, result

    pending 'decode with too many spaces', ->
      result = AtbashCipher.decode 'vc vix    r hn'
      expected = 'exercism'
      assert.are.equal expected, result

    pending 'decode with no spaces', ->
      result = AtbashCipher.decode 'zmlyhgzxovrhlugvmzhgvkkrmthglmv'
      expected = 'anobstacleisoftenasteppingstone'
      assert.are.equal expected, result
