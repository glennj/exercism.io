 import gamestate from require 'state_of_tic_tac_toe'

describe 'state-of-tic-tac-toe', ->
  describe 'Won games', ->
    it 'Finished game where X won via left column victory', ->
      board = {
        'XOO',
        'X  ',
        'X  ',
      }
      assert.are.equal 'win', gamestate board

    pending 'Finished game where X won via middle column victory', ->
      board = {
        'OXO',
        ' X ',
        ' X ',
      }
      assert.are.equal 'win', gamestate board

    pending 'Finished game where X won via right column victory', ->
      board = {
        'OOX',
        '  X',
        '  X',
      }
      assert.are.equal 'win', gamestate board

    pending 'Finished game where O won via left column victory', ->
      board = {
        'OXX',
        'OX ',
        'O  ',
      }
      assert.are.equal 'win', gamestate board

    pending 'Finished game where O won via middle column victory', ->
      board = {
        'XOX',
        ' OX',
        ' O ',
      }
      assert.are.equal 'win', gamestate board

    pending 'Finished game where O won via right column victory', ->
      board = {
        'XXO',
        ' XO',
        '  O',
      }
      assert.are.equal 'win', gamestate board

    pending 'Finished game where X won via top row victory', ->
      board = {
        'XXX',
        'XOO',
        'O  ',
      }
      assert.are.equal 'win', gamestate board

    pending 'Finished game where X won via middle row victory', ->
      board = {
        'O  ',
        'XXX',
        ' O ',
      }
      assert.are.equal 'win', gamestate board

    pending 'Finished game where X won via bottom row victory', ->
      board = {
        ' OO',
        'O X',
        'XXX',
      }
      assert.are.equal 'win', gamestate board

    pending 'Finished game where O won via top row victory', ->
      board = {
        'OOO',
        'XXO',
        'XX ',
      }
      assert.are.equal 'win', gamestate board

    pending 'Finished game where O won via middle row victory', ->
      board = {
        'XX ',
        'OOO',
        'X  ',
      }
      assert.are.equal 'win', gamestate board

    pending 'Finished game where O won via bottom row victory', ->
      board = {
        'XOX',
        ' XX',
        'OOO',
      }
      assert.are.equal 'win', gamestate board

    pending 'Finished game where X won via falling diagonal victory', ->
      board = {
        'XOO',
        ' X ',
        '  X',
      }
      assert.are.equal 'win', gamestate board

    pending 'Finished game where X won via rising diagonal victory', ->
      board = {
        'O X',
        'OX ',
        'X  ',
      }
      assert.are.equal 'win', gamestate board

    pending 'Finished game where O won via falling diagonal victory', ->
      board = {
        'OXX',
        'OOX',
        'X O',
      }
      assert.are.equal 'win', gamestate board

    pending 'Finished game where O won via rising diagonal victory', ->
      board = {
        '  O',
        ' OX',
        'OXX',
      }
      assert.are.equal 'win', gamestate board

    pending 'Finished game where X won via a row and a column victory', ->
      board = {
        'XXX',
        'XOO',
        'XOO',
      }
      assert.are.equal 'win', gamestate board

    pending 'Finished game where X won via two diagonal victories', ->
      board = {
        'XOX',
        'OXO',
        'XOX',
      }
      assert.are.equal 'win', gamestate board

  describe 'Drawn games', ->
    pending 'Draw', ->
      board = {
        'XOX',
        'XXO',
        'OXO',
      }
      assert.are.equal 'draw', gamestate board

    pending 'Another draw', ->
      board = {
        'XXO',
        'OXX',
        'XOO',
      }
      assert.are.equal 'draw', gamestate board

  describe 'Ongoing games', ->
    pending 'Ongoing game: one move in', ->
      board = {
        '   ',
        'X  ',
        '   ',
      }
      assert.are.equal 'ongoing', gamestate board

    pending 'Ongoing game: two moves in', ->
      board = {
        'O  ',
        ' X ',
        '   ',
      }
      assert.are.equal 'ongoing', gamestate board

    pending 'Ongoing game: five moves in', ->
      board = {
        'X  ',
        ' XO',
        'OX ',
      }
      assert.are.equal 'ongoing', gamestate board

  describe 'Invalid boards', ->
    pending 'Invalid board: X went twice', ->
      board = {
        'XX ',
        '   ',
        '   ',
      }
      f = -> gamestate board
      assert.has.error f, 'Wrong turn order: X went twice'

    pending 'Invalid board: O started', ->
      board = {
        'OOX',
        '   ',
        '   ',
      }
      f = -> gamestate board
      assert.has.error f, 'Wrong turn order: O started'

    pending 'Invalid board: X won and O kept playing', ->
      board = {
        'XXX',
        'OOO',
        '   ',
      }
      f = -> gamestate board
      assert.has.error f, 'Impossible board: game should have ended after the game was won'

    pending 'Invalid board: players kept playing after a win', ->
      board = {
        'XXX',
        'OOO',
        'XOX',
      }
      f = -> gamestate board
      assert.has.error f, 'Impossible board: game should have ended after the game was won'
