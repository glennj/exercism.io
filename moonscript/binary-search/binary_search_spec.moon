BinarySearch = require 'binary_search'

describe 'binary-search', ->
  -- Remember, Lua (and MoonScript) use 1-based indexing.
  
  it 'finds a value in an array with one element', ->
    list = {6}
    index = BinarySearch.find list, 6
    assert.are.equal 1, index

  it 'finds a value in the middle of an array', ->
    list = {1, 3, 4, 6, 8, 9, 11}
    index = BinarySearch.find list, 6
    assert.are.equal 4, index

  it 'finds a value at the beginning of an array', ->
    list = {1, 3, 4, 6, 8, 9, 11}
    index = BinarySearch.find list, 1
    assert.are.equal 1, index

  it 'finds a value at the end of an array', ->
    list = {1, 3, 4, 6, 8, 9, 11}
    index = BinarySearch.find list, 11
    assert.are.equal 7, index

  it 'finds a value in an array of odd length', ->
    list = {1, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 634}
    index = BinarySearch.find list, 144
    assert.are.equal 10, index

  it 'finds a value in an array of even length', ->
    list = {1, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377}
    index = BinarySearch.find list, 21
    assert.are.equal 6, index

  it 'identifies that a value is not included in the array', ->
    list = {1, 3, 4, 6, 8, 9, 11}
    f = -> BinarySearch.find list, 7
    assert.has.errors f, 'value not in array'

  it "a value smaller than the array's smallest value is not found", ->
    list = {1, 3, 4, 6, 8, 9, 11}
    f = -> BinarySearch.find list, 0
    assert.has.errors f, 'value not in array'

  it "a value larger than the array's largest value is not found", ->
    list = {1, 3, 4, 6, 8, 9, 11}
    f = -> BinarySearch.find list, 13
    assert.has.errors f, 'value not in array'

  it 'nothing is found in an empty array', ->
    list = {}
    f = -> BinarySearch.find list, 1
    assert.has.errors f, 'value not in array'

  it 'nothing is found when the left and right bounds cross', ->
    list = {1, 2}
    f = -> BinarySearch.find list, 0
    assert.has.errors f, 'value not in array'
