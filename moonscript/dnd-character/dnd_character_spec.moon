import modifier, ability, character from require 'dnd_character'

describe 'dnd-character', ->
  -- ----------------------------------------------------------
  between = (state, arguments) ->
    assert #arguments == 3, 'expected three arguments to assert.between: value, min, max'
    { val, min, max } = arguments
    val and min <= val and val <= max

  say = require 'say'
  say\set 'assertion.between.positive', 'Expected %s to be in the range [%s, %s]'
  say\set 'assertion.between.negative', 'Expected %s not to be in the range [%s, %s]'
  assert\register 'assertion', 'between', between, 'assertion.between.positive', 'assertion.between.negative'
  -- ----------------------------------------------------------
  
  describe 'ability modifier', ->
    it 'ability modifier for score 3 is -4', ->
      assert.are.equal -4, modifier 3

    it 'ability modifier for score 4 is -3', ->
      assert.are.equal -3, modifier 4

    it 'ability modifier for score 5 is -3', ->
      assert.are.equal -3, modifier 5

    it 'ability modifier for score 6 is -2', ->
      assert.are.equal -2, modifier 6

    it 'ability modifier for score 7 is -2', ->
      assert.are.equal -2, modifier 7

    it 'ability modifier for score 8 is -1', ->
      assert.are.equal -1, modifier 8

    it 'ability modifier for score 9 is -1', ->
      assert.are.equal -1, modifier 9

    it 'ability modifier for score 10 is 0', ->
      assert.are.equal 0, modifier 10

    it 'ability modifier for score 11 is 0', ->
      assert.are.equal 0, modifier 11

    it 'ability modifier for score 12 is +1', ->
      assert.are.equal 1, modifier 12

    it 'ability modifier for score 13 is +1', ->
      assert.are.equal 1, modifier 13

    it 'ability modifier for score 14 is +2', ->
      assert.are.equal 2, modifier 14

    it 'ability modifier for score 15 is +2', ->
      assert.are.equal 2, modifier 15

    it 'ability modifier for score 16 is +3', ->
      assert.are.equal 3, modifier 16

    it 'ability modifier for score 17 is +3', ->
      assert.are.equal 3, modifier 17

    it 'ability modifier for score 18 is +4', ->
      assert.are.equal 4, modifier 18

  it 'random ability is within range', ->
    for i = 1, 50
      score = ability!
      assert.between score, 3, 18

  it 'random character is valid', ->
    player = character!
    assert.between player.strength, 3, 18
    assert.between player.dexterity, 3, 18
    assert.between player.constitution, 3, 18
    assert.between player.intelligence, 3, 18
    assert.between player.wisdom, 3, 18
    assert.between player.charisma, 3, 18
    assert.are.equal (10 + modifier player.constitution), player.hitpoints

  it 'each ability is only calculated once', ->
    player = character!
    assert.are.equal player.strength, player.strength
    assert.are.equal player.dexterity, player.dexterity
    assert.are.equal player.constitution, player.constitution
    assert.are.equal player.intelligence, player.intelligence
    assert.are.equal player.wisdom, player.wisdom
    assert.are.equal player.charisma, player.charisma
    assert.are.equal player.hitpoints, player.hitpoints
