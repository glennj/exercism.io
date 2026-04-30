import maximumValue from require 'knapsack'

describe 'knapsack', ->
  it 'no items', ->
    items = {}
    maxWeight = 100
    result = maximumValue maxWeight, items
    assert.are.equal 0, result

  it 'one item, too heavy', ->
    items = {{value: 1, weight: 100}}
    maxWeight = 10
    result = maximumValue maxWeight, items
    assert.are.equal 0, result

  it 'five items (cannot be greedy by weight)', ->
    items = {
      {value: 5, weight: 2},
      {value: 5, weight: 2},
      {value: 5, weight: 2},
      {value: 5, weight: 2},
      {value: 21, weight: 10},
    }
    maxWeight = 10
    result = maximumValue maxWeight, items
    assert.are.equal 21, result

  it 'five items (cannot be greedy by value)', ->
    items = {
      {value: 20, weight: 2},
      {value: 20, weight: 2},
      {value: 20, weight: 2},
      {value: 20, weight: 2},
      {value: 50, weight: 10},
    }
    maxWeight = 10
    result = maximumValue maxWeight, items
    assert.are.equal 80, result

  it 'example knapsack', ->
    items = {
      {value: 10, weight: 5},
      {value: 40, weight: 4},
      {value: 30, weight: 6},
      {value: 50, weight: 4},
    }
    maxWeight = 10
    result = maximumValue maxWeight, items
    assert.are.equal 90, result

  it '8 items', ->
    items = {
      {value: 350, weight: 25},
      {value: 400, weight: 35},
      {value: 450, weight: 45},
      {value: 20, weight: 5},
      {value: 70, weight: 25},
      {value: 8, weight: 3},
      {value: 5, weight: 2},
      {value: 5, weight: 2},
    }
    maxWeight = 104
    result = maximumValue maxWeight, items
    assert.are.equal 900, result

  it '15 items', ->
    items = {
      {value: 135, weight: 70},
      {value: 139, weight: 73},
      {value: 149, weight: 77},
      {value: 150, weight: 80},
      {value: 156, weight: 82},
      {value: 163, weight: 87},
      {value: 173, weight: 90},
      {value: 184, weight: 94},
      {value: 192, weight: 98},
      {value: 201, weight: 106},
      {value: 210, weight: 110},
      {value: 214, weight: 113},
      {value: 221, weight: 115},
      {value: 229, weight: 118},
      {value: 240, weight: 120},
    }
    maxWeight = 750
    result = maximumValue maxWeight, items
    assert.are.equal 1458, result
