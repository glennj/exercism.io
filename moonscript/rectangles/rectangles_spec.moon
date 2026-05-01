import rectangles from require 'rectangles'

describe 'rectangles', ->
  it 'no rows', ->
    input = {}
    assert.are.equal 0, rectangles input

  it 'no columns', ->
    input = {''}
    assert.are.equal 0, rectangles input

  it 'no rectangles', ->
    input = {' '}
    assert.are.equal 0, rectangles input

  it 'one rectangle', ->
    input = {
      '+-+',
      '| |',
      '+-+',
    }
    assert.are.equal 1, rectangles input

  it 'two rectangles without shared parts', ->
    input = {
      '  +-+',
      '  | |',
      '+-+-+',
      '| |  ',
      '+-+  ',
    }
    assert.are.equal 2, rectangles input

  it 'five rectangles with shared parts', ->
    input = {
      '  +-+',
      '  | |',
      '+-+-+',
      '| | |',
      '+-+-+',
    }
    assert.are.equal 5, rectangles input

  it 'rectangle of height 1 is counted', ->
    input = {
      '+--+',
      '+--+',
    }
    assert.are.equal 1, rectangles input

  it 'rectangle of width 1 is counted', ->
    input = {
      '++',
      '||',
      '++',
    }
    assert.are.equal 1, rectangles input

  it '1x1 square is counted', ->
    input = {
      '++',
      '++',
    }
    assert.are.equal 1, rectangles input

  it 'only complete rectangles are counted', ->
    input = {
      '  +-+',
      '    |',
      '+-+-+',
      '| | -',
      '+-+-+',
    }
    assert.are.equal 1, rectangles input

  it 'rectangles can be of different sizes', ->
    input = {
      '+------+----+',
      '|      |    |',
      '+---+--+    |',
      '|   |       |',
      '+---+-------+',
    }
    assert.are.equal 3, rectangles input

  it 'corner is required for a rectangle to be complete', ->
    input = {
      '+------+----+',
      '|      |    |',
      '+------+    |',
      '|   |       |',
      '+---+-------+',
    }
    assert.are.equal 2, rectangles input

  it 'large input with many rectangles', ->
    input = {
      '+---+--+----+',
      '|   +--+----+',
      '+---+--+    |',
      '|   +--+----+',
      '+---+--+--+-+',
      '+---+--+--+-+',
      '+------+  | |',
      '          +-+',
    }
    assert.are.equal 60, rectangles input

  it 'rectangles must have four sides', ->
    input = {
      '+-+ +-+',
      '| | | |',
      '+-+-+-+',
      '  | |  ',
      '+-+-+-+',
      '| | | |',
      '+-+ +-+',
    }
    assert.are.equal 5, rectangles input
