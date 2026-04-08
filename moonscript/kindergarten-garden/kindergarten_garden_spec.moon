KindergartenGarden = require 'kindergarten_garden'

describe 'kindergarten-garden', ->
  describe 'partial garden', ->
    it 'garden with single student', ->
      garden = KindergartenGarden "RC\nGG"
      result = garden\plants 'Alice'
      expected = {'radishes', 'clover', 'grass', 'grass'}
      assert.are.same expected, result

    it 'different garden with single student', ->
      garden = KindergartenGarden "VC\nRC"
      result = garden\plants 'Alice'
      expected = {'violets', 'clover', 'radishes', 'clover'}
      assert.are.same expected, result

    it 'garden with two students', ->
      garden = KindergartenGarden "VVCG\nVVRC"
      result = garden\plants 'Bob'
      expected = {'clover', 'grass', 'radishes', 'clover'}
      assert.are.same expected, result

    it 'multiple students for the same garden with three students', ->
      garden = KindergartenGarden "VVCCGG\nVVCCGG"

      result = garden\plants 'Bob'
      expected = {'clover', 'clover', 'clover', 'clover'}
      assert.are.same expected, result

      result = garden\plants 'Charlie'
      expected = {'grass', 'grass', 'grass', 'grass'}
      assert.are.same expected, result

  describe 'full garden', ->
    it 'all students', ->
      garden = KindergartenGarden "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV"

      result = garden\plants 'Alice'
      expected = {'violets', 'radishes', 'violets', 'radishes'}
      assert.are.same expected, result

      result = garden\plants 'Bob'
      expected = {'clover', 'grass', 'clover', 'clover'}
      assert.are.same expected, result

      result = garden\plants 'Charlie'
      expected = {'violets', 'violets', 'clover', 'grass'}
      assert.are.same expected, result

      result = garden\plants 'David'
      expected = {'radishes', 'violets', 'clover', 'radishes'}
      assert.are.same expected, result

      result = garden\plants 'Eve'
      expected = {'clover', 'grass', 'radishes', 'grass'}
      assert.are.same expected, result

      result = garden\plants 'Fred'
      expected = {'grass', 'clover', 'violets', 'clover'}
      assert.are.same expected, result

      result = garden\plants 'Ginny'
      expected = {'clover', 'grass', 'grass', 'clover'}
      assert.are.same expected, result

      result = garden\plants 'Harriet'
      expected = {'violets', 'radishes', 'radishes', 'violets'}
      assert.are.same expected, result

      result = garden\plants 'Ileana'
      expected = {'grass', 'clover', 'violets', 'clover'}
      assert.are.same expected, result

      result = garden\plants 'Joseph'
      expected = {'violets', 'clover', 'violets', 'grass'}
      assert.are.same expected, result

      result = garden\plants 'Kincaid'
      expected = {'grass', 'clover', 'clover', 'grass'}
      assert.are.same expected, result

      result = garden\plants 'Larry'
      expected = {'grass', 'violets', 'clover', 'violets'}
      assert.are.same expected, result
