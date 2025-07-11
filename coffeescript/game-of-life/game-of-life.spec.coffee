GameOfLife = require './game-of-life'

describe 'Game Of Life', ->
  it 'Empty matrix', ->
    game = new GameOfLife []
    game.tick()
    expect(game.matrix).toEqual []

  it 'Live cells with zero live neighbors die', ->
    game = new GameOfLife [
      [0, 0, 0]
      [0, 1, 0]
      [0, 0, 0]
    ]
    game.tick()
    expect(game.matrix).toEqual [
      [0, 0, 0]
      [0, 0, 0]
      [0, 0, 0]
    ]

  it 'Live cells with only one live neighbor die', ->
    game = new GameOfLife [
      [0, 0, 0]
      [0, 1, 0]
      [0, 1, 0]
    ]
    game.tick()
    expect(game.matrix).toEqual [
      [0, 0, 0]
      [0, 0, 0]
      [0, 0, 0]
    ]

  it 'Live cells with two live neighbors stay alive', ->
    game = new GameOfLife [
      [1, 0, 1]
      [1, 0, 1]
      [1, 0, 1]
    ]
    game.tick()
    expect(game.matrix).toEqual [
      [0, 0, 0]
      [1, 0, 1]
      [0, 0, 0]
    ]

  it 'Live cells with three live neighbors stay alive', ->
    game = new GameOfLife [
      [0, 1, 0]
      [1, 0, 0]
      [1, 1, 0]
    ]
    game.tick()
    expect(game.matrix).toEqual [
      [0, 0, 0]
      [1, 0, 0]
      [1, 1, 0]
    ]

  it 'Dead cells with three live neighbors become alive', ->
    game = new GameOfLife [
      [1, 1, 0]
      [0, 0, 0]
      [1, 0, 0]
    ]
    game.tick()
    expect(game.matrix).toEqual [
      [0, 0, 0]
      [1, 1, 0]
      [0, 0, 0]
    ]

  it 'Live cells with four or more neighbors die', ->
    game = new GameOfLife [
      [1, 1, 1]
      [1, 1, 1]
      [1, 1, 1]
    ]
    game.tick()
    expect(game.matrix).toEqual [
      [1, 0, 1]
      [0, 0, 0]
      [1, 0, 1]
    ]

  it 'Bigger matrix', ->
    game = new GameOfLife [
      [1, 1, 0, 1, 1, 0, 0, 0]
      [1, 0, 1, 1, 0, 0, 0, 0]
      [1, 1, 1, 0, 0, 1, 1, 1]
      [0, 0, 0, 0, 0, 1, 1, 0]
      [1, 0, 0, 0, 1, 1, 0, 0]
      [1, 1, 0, 0, 0, 1, 1, 1]
      [0, 0, 1, 0, 1, 0, 0, 1]
      [1, 0, 0, 0, 0, 0, 1, 1]
    ]
    game.tick()
    expect(game.matrix).toEqual [
      [1, 1, 0, 1, 1, 0, 0, 0]
      [0, 0, 0, 0, 0, 1, 1, 0]
      [1, 0, 1, 1, 1, 1, 0, 1]
      [1, 0, 0, 0, 0, 0, 0, 1]
      [1, 1, 0, 0, 1, 0, 0, 1]
      [1, 1, 0, 1, 0, 0, 0, 1]
      [1, 0, 0, 0, 0, 0, 0, 0]
      [0, 0, 0, 0, 0, 0, 1, 1]
    ]
