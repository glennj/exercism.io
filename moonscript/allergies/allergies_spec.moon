Allergies = require 'allergies'

describe 'allergies', ->
  describe 'testing for eggs allergy', ->
    it 'not allergic to anything', ->
      result = Allergies.allergic_to 'eggs', 0
      assert.is_false result

    it 'allergic only to eggs', ->
      result = Allergies.allergic_to 'eggs', 1
      assert.is_true result

    it 'allergic to eggs and something else', ->
      result = Allergies.allergic_to 'eggs', 3
      assert.is_true result

    it 'allergic to something, but not eggs', ->
      result = Allergies.allergic_to 'eggs', 2
      assert.is_false result

    it 'allergic to everything', ->
      result = Allergies.allergic_to 'eggs', 255
      assert.is_true result

  describe 'testing for peanuts allergy', ->
    it 'not allergic to anything', ->
      result = Allergies.allergic_to 'peanuts', 0
      assert.is_false result

    it 'allergic only to peanuts', ->
      result = Allergies.allergic_to 'peanuts', 2
      assert.is_true result

    it 'allergic to peanuts and something else', ->
      result = Allergies.allergic_to 'peanuts', 7
      assert.is_true result

    it 'allergic to something, but not peanuts', ->
      result = Allergies.allergic_to 'peanuts', 5
      assert.is_false result

    it 'allergic to everything', ->
      result = Allergies.allergic_to 'peanuts', 255
      assert.is_true result

  describe 'testing for shellfish allergy', ->
    it 'not allergic to anything', ->
      result = Allergies.allergic_to 'shellfish', 0
      assert.is_false result

    it 'allergic only to shellfish', ->
      result = Allergies.allergic_to 'shellfish', 4
      assert.is_true result

    it 'allergic to shellfish and something else', ->
      result = Allergies.allergic_to 'shellfish', 14
      assert.is_true result

    it 'allergic to something, but not shellfish', ->
      result = Allergies.allergic_to 'shellfish', 10
      assert.is_false result

    it 'allergic to everything', ->
      result = Allergies.allergic_to 'shellfish', 255
      assert.is_true result

  describe 'testing for strawberries allergy', ->
    it 'not allergic to anything', ->
      result = Allergies.allergic_to 'strawberries', 0
      assert.is_false result

    it 'allergic only to strawberries', ->
      result = Allergies.allergic_to 'strawberries', 8
      assert.is_true result

    it 'allergic to strawberries and something else', ->
      result = Allergies.allergic_to 'strawberries', 28
      assert.is_true result

    it 'allergic to something, but not strawberries', ->
      result = Allergies.allergic_to 'strawberries', 20
      assert.is_false result

    it 'allergic to everything', ->
      result = Allergies.allergic_to 'strawberries', 255
      assert.is_true result

  describe 'testing for tomatoes allergy', ->
    it 'not allergic to anything', ->
      result = Allergies.allergic_to 'tomatoes', 0
      assert.is_false result

    it 'allergic only to tomatoes', ->
      result = Allergies.allergic_to 'tomatoes', 16
      assert.is_true result

    it 'allergic to tomatoes and something else', ->
      result = Allergies.allergic_to 'tomatoes', 56
      assert.is_true result

    it 'allergic to something, but not tomatoes', ->
      result = Allergies.allergic_to 'tomatoes', 40
      assert.is_false result

    it 'allergic to everything', ->
      result = Allergies.allergic_to 'tomatoes', 255
      assert.is_true result

  describe 'testing for chocolate allergy', ->
    it 'not allergic to anything', ->
      result = Allergies.allergic_to 'chocolate', 0
      assert.is_false result

    it 'allergic only to chocolate', ->
      result = Allergies.allergic_to 'chocolate', 32
      assert.is_true result

    it 'allergic to chocolate and something else', ->
      result = Allergies.allergic_to 'chocolate', 112
      assert.is_true result

    it 'allergic to something, but not chocolate', ->
      result = Allergies.allergic_to 'chocolate', 80
      assert.is_false result

    it 'allergic to everything', ->
      result = Allergies.allergic_to 'chocolate', 255
      assert.is_true result

  describe 'testing for pollen allergy', ->
    it 'not allergic to anything', ->
      result = Allergies.allergic_to 'pollen', 0
      assert.is_false result

    it 'allergic only to pollen', ->
      result = Allergies.allergic_to 'pollen', 64
      assert.is_true result

    it 'allergic to pollen and something else', ->
      result = Allergies.allergic_to 'pollen', 224
      assert.is_true result

    it 'allergic to something, but not pollen', ->
      result = Allergies.allergic_to 'pollen', 160
      assert.is_false result

    it 'allergic to everything', ->
      result = Allergies.allergic_to 'pollen', 255
      assert.is_true result

  describe 'testing for cats allergy', ->
    it 'not allergic to anything', ->
      result = Allergies.allergic_to 'cats', 0
      assert.is_false result

    it 'allergic only to cats', ->
      result = Allergies.allergic_to 'cats', 128
      assert.is_true result

    it 'allergic to cats and something else', ->
      result = Allergies.allergic_to 'cats', 192
      assert.is_true result

    it 'allergic to something, but not cats', ->
      result = Allergies.allergic_to 'cats', 64
      assert.is_false result

    it 'allergic to everything', ->
      result = Allergies.allergic_to 'cats', 255
      assert.is_true result

  describe 'list when:', ->
    it 'no allergies', ->
      result = Allergies.list 0
      expected = {}
      assert.is.same expected, result

    it 'just eggs', ->
      result = Allergies.list 1
      expected = {'eggs'}
      assert.is.same expected, result

    it 'just peanuts', ->
      result = Allergies.list 2
      expected = {'peanuts'}
      assert.is.same expected, result

    it 'just strawberries', ->
      result = Allergies.list 8
      expected = {'strawberries'}
      assert.is.same expected, result

    it 'eggs and peanuts', ->
      result = Allergies.list 3
      expected = {'eggs', 'peanuts'}
      assert.is.same expected, result

    it 'more than eggs but not peanuts', ->
      result = Allergies.list 5
      expected = {'eggs', 'shellfish'}
      assert.is.same expected, result

    it 'lots of stuff', ->
      result = Allergies.list 248
      expected = {'strawberries', 'tomatoes', 'chocolate', 'pollen', 'cats'}
      assert.is.same expected, result

    it 'everything', ->
      result = Allergies.list 255
      expected = {'eggs', 'peanuts', 'shellfish', 'strawberries', 'tomatoes', 'chocolate', 'pollen', 'cats'}
      assert.is.same expected, result

    it 'no allergen score parts', ->
      result = Allergies.list 509
      expected = {'eggs', 'shellfish', 'strawberries', 'tomatoes', 'chocolate', 'pollen', 'cats'}
      assert.is.same expected, result

    it 'no allergen score parts without highest valid score', ->
      result = Allergies.list 257
      expected = {'eggs'}
      assert.is.same expected, result
