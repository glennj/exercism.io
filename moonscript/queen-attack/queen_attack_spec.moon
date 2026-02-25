Queen = require 'queen_attack'

describe 'queen-attack', ->
  -- Valid values for rows and columns are: 0 to 7 inclusive.

  describe 'Test creation of Queens with valid and invalid positions', ->
    it 'queen with a valid position', ->
      queen = Queen 2, 2
      assert.is.truthy queen

    it 'queen must have positive row', ->
      f = -> Queen -2, 2
      assert.has.errors f, 'invalid position'

    it 'queen must have row on board', ->
      f = -> Queen 8, 4
      assert.has.errors f, 'invalid position'

    it 'queen must have positive column', ->
      f = -> Queen 2, -2
      assert.has.errors f, 'invalid position'

    it 'queen must have column on board', ->
      f = -> Queen 4, 8
      assert.has.errors f, 'invalid position'

  describe 'Test the ability of one queen to attack another', ->
    it 'cannot attack', ->
      black_queen = Queen 6, 6
      white_queen = Queen 2, 4
      assert.is_false black_queen\can_attack white_queen

    it 'can attack on same row', ->
      black_queen = Queen 2, 6
      white_queen = Queen 2, 4
      assert.is_true black_queen\can_attack white_queen

    it 'can attack on same column', ->
      black_queen = Queen 2, 5
      white_queen = Queen 4, 5
      assert.is_true black_queen\can_attack white_queen

    it 'can attack on first diagonal', ->
      black_queen = Queen 0, 4
      white_queen = Queen 2, 2
      assert.is_true black_queen\can_attack white_queen

    it 'can attack on second diagonal', ->
      black_queen = Queen 3, 1
      white_queen = Queen 2, 2
      assert.is_true black_queen\can_attack white_queen

    it 'can attack on third diagonal', ->
      black_queen = Queen 1, 1
      white_queen = Queen 2, 2
      assert.is_true black_queen\can_attack white_queen

    it 'can attack on fourth diagonal', ->
      black_queen = Queen 0, 6
      white_queen = Queen 1, 7
      assert.is_true black_queen\can_attack white_queen

    it 'cannot attack if falling diagonals are only the same when reflected across the longest falling diagonal', ->
      black_queen = Queen 2, 5
      white_queen = Queen 4, 1
      assert.is_false black_queen\can_attack white_queen
