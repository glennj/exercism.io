RailFenceCipher = require 'rail_fence_cipher'

describe 'rail-fence-cipher', ->
  describe 'encode', ->
    it 'encode with two rails', ->
      result = RailFenceCipher.encode 2, 'XOXOXOXOXOXOXOXOXO'
      expected = 'XXXXXXXXXOOOOOOOOO'
      assert.are.equal expected, result

    it 'encode with three rails', ->
      result = RailFenceCipher.encode 3, 'WEAREDISCOVEREDFLEEATONCE'
      expected = 'WECRLTEERDSOEEFEAOCAIVDEN'
      assert.are.equal expected, result

    it 'encode with ending in the middle', ->
      result = RailFenceCipher.encode 4, 'EXERCISES'
      expected = 'ESXIEECSR'
      assert.are.equal expected, result

  describe 'decode', ->
    it 'decode with three rails', ->
      result = RailFenceCipher.decode 3, 'TEITELHDVLSNHDTISEIIEA'
      expected = 'THEDEVILISINTHEDETAILS'
      assert.are.equal expected, result

    it 'decode with five rails', ->
      result = RailFenceCipher.decode 5, 'EIEXMSMESAORIWSCE'
      expected = 'EXERCISMISAWESOME'
      assert.are.equal expected, result

    it 'decode with six rails', ->
      result = RailFenceCipher.decode 6, '133714114238148966225439541018335470986172518171757571896261'
      expected = '112358132134558914423337761098715972584418167651094617711286'
      assert.are.equal expected, result
