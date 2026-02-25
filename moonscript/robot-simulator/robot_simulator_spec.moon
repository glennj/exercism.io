Robot = require 'robot_simulator'

describe 'robot-simulator', ->
  describe 'Create robot', ->
    it 'at origin facing north', ->
      robot = Robot x: 0, y: 0, direction: 'north'
      assert.are.equal 0, robot\x!
      assert.are.equal 0, robot\y!
      assert.are.equal 'north', robot\direction!

    it 'at negative position facing south', ->
      robot = Robot x: -1, y: -1, direction: 'south'
      assert.are.equal -1, robot\x!
      assert.are.equal -1, robot\y!
      assert.are.equal 'south', robot\direction!

  describe 'Rotating clockwise', ->
    it 'changes north to east', ->
      robot = Robot x: 0, y: 0, direction: 'north'
      robot\move 'R'
      assert.are.equal 0, robot\x!
      assert.are.equal 0, robot\y!
      assert.are.equal 'east', robot\direction!

    it 'changes east to south', ->
      robot = Robot x: 0, y: 0, direction: 'east'
      robot\move 'R'
      assert.are.equal 0, robot\x!
      assert.are.equal 0, robot\y!
      assert.are.equal 'south', robot\direction!

    it 'changes south to west', ->
      robot = Robot x: 0, y: 0, direction: 'south'
      robot\move 'R'
      assert.are.equal 0, robot\x!
      assert.are.equal 0, robot\y!
      assert.are.equal 'west', robot\direction!

    it 'changes west to north', ->
      robot = Robot x: 0, y: 0, direction: 'west'
      robot\move 'R'
      assert.are.equal 0, robot\x!
      assert.are.equal 0, robot\y!
      assert.are.equal 'north', robot\direction!

  describe 'Rotating counter-clockwise', ->
    it 'changes north to west', ->
      robot = Robot x: 0, y: 0, direction: 'north'
      robot\move 'L'
      assert.are.equal 0, robot\x!
      assert.are.equal 0, robot\y!
      assert.are.equal 'west', robot\direction!

    it 'changes west to south', ->
      robot = Robot x: 0, y: 0, direction: 'west'
      robot\move 'L'
      assert.are.equal 0, robot\x!
      assert.are.equal 0, robot\y!
      assert.are.equal 'south', robot\direction!

    it 'changes south to east', ->
      robot = Robot x: 0, y: 0, direction: 'south'
      robot\move 'L'
      assert.are.equal 0, robot\x!
      assert.are.equal 0, robot\y!
      assert.are.equal 'east', robot\direction!

    it 'changes east to north', ->
      robot = Robot x: 0, y: 0, direction: 'east'
      robot\move 'L'
      assert.are.equal 0, robot\x!
      assert.are.equal 0, robot\y!
      assert.are.equal 'north', robot\direction!

  describe 'Moving forward one', ->
    it 'facing north increments Y', ->
      robot = Robot x: 0, y: 0, direction: 'north'
      robot\move 'A'
      assert.are.equal 0, robot\x!
      assert.are.equal 1, robot\y!
      assert.are.equal 'north', robot\direction!

    it 'facing south decrements Y', ->
      robot = Robot x: 0, y: 0, direction: 'south'
      robot\move 'A'
      assert.are.equal 0, robot\x!
      assert.are.equal -1, robot\y!
      assert.are.equal 'south', robot\direction!

    it 'facing east increments X', ->
      robot = Robot x: 0, y: 0, direction: 'east'
      robot\move 'A'
      assert.are.equal 1, robot\x!
      assert.are.equal 0, robot\y!
      assert.are.equal 'east', robot\direction!

    it 'facing west decrements X', ->
      robot = Robot x: 0, y: 0, direction: 'west'
      robot\move 'A'
      assert.are.equal -1, robot\x!
      assert.are.equal 0, robot\y!
      assert.are.equal 'west', robot\direction!

  describe 'Follow series of instructions', ->
    it 'moving east and north from README', ->
      robot = Robot x: 7, y: 3, direction: 'north'
      robot\move 'RAALAL'
      assert.are.equal 9, robot\x!
      assert.are.equal 4, robot\y!
      assert.are.equal 'west', robot\direction!

    it 'moving west and north', ->
      robot = Robot x: 0, y: 0, direction: 'north'
      robot\move 'LAAARALA'
      assert.are.equal -4, robot\x!
      assert.are.equal 1, robot\y!
      assert.are.equal 'west', robot\direction!

    it 'moving west and south', ->
      robot = Robot x: 2, y: -7, direction: 'east'
      robot\move 'RRAAAAALA'
      assert.are.equal -3, robot\x!
      assert.are.equal -8, robot\y!
      assert.are.equal 'south', robot\direction!

    it 'moving east and north', ->
      robot = Robot x: 8, y: 4, direction: 'south'
      robot\move 'LAAARRRALLLL'
      assert.are.equal 11, robot\x!
      assert.are.equal 5, robot\y!
      assert.are.equal 'north', robot\direction!
