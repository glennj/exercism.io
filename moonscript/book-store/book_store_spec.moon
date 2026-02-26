BookStore = require 'book_store'

describe 'book-store', ->
  it 'Only a single book', ->
    result = BookStore.total {1}
    expected = 800
    assert.are.equal expected, result

  it 'Two of the same book', ->
    result = BookStore.total {2, 2}
    expected = 1600
    assert.are.equal expected, result

  it 'Empty basket', ->
    result = BookStore.total {}
    expected = 0
    assert.are.equal expected, result

  it 'Two different books', ->
    result = BookStore.total {1, 2}
    expected = 1520
    assert.are.equal expected, result

  it 'Three different books', ->
    result = BookStore.total {1, 2, 3}
    expected = 2160
    assert.are.equal expected, result

  it 'Four different books', ->
    result = BookStore.total {1, 2, 3, 4}
    expected = 2560
    assert.are.equal expected, result

  it 'Five different books', ->
    result = BookStore.total {1, 2, 3, 4, 5}
    expected = 3000
    assert.are.equal expected, result

  it 'Two groups of four is cheaper than group of five plus group of three', ->
    result = BookStore.total {1, 1, 2, 2, 3, 3, 4, 5}
    expected = 5120
    assert.are.equal expected, result

  it 'Two groups of four is cheaper than groups of five and three', ->
    result = BookStore.total {1, 1, 2, 3, 4, 4, 5, 5}
    expected = 5120
    assert.are.equal expected, result

  it 'Group of four plus group of two is cheaper than two groups of three', ->
    result = BookStore.total {1, 1, 2, 2, 3, 4}
    expected = 4080
    assert.are.equal expected, result

  it 'Two each of first four books and one copy each of rest', ->
    result = BookStore.total {1, 1, 2, 2, 3, 3, 4, 4, 5}
    expected = 5560
    assert.are.equal expected, result

  it 'Two copies of each book', ->
    result = BookStore.total {1, 1, 2, 2, 3, 3, 4, 4, 5, 5}
    expected = 6000
    assert.are.equal expected, result

  it 'Three copies of first book and two each of remaining', ->
    result = BookStore.total {1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 1}
    expected = 6800
    assert.are.equal expected, result

  it 'Three each of first two books and two each of remaining books', ->
    result = BookStore.total {1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 1, 2}
    expected = 7520
    assert.are.equal expected, result

  it 'Four groups of four are cheaper than two groups each of five and three', ->
    result = BookStore.total {1, 1, 2, 2, 3, 3, 4, 5, 1, 1, 2, 2, 3, 3, 4, 5}
    expected = 10240
    assert.are.equal expected, result

  it 'Check that groups of four are created properly even when there are more groups of three than groups of five', ->
    result = BookStore.total {1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 5, 5}
    expected = 14560
    assert.are.equal expected, result

  it 'One group of one and four is cheaper than one group of two and three', ->
    result = BookStore.total {1, 1, 2, 3, 4}
    expected = 3360
    assert.are.equal expected, result

  it 'One group of one and two plus three groups of four is cheaper than one group of each size', ->
    result = BookStore.total {1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5}
    expected = 10000
    assert.are.equal expected, result
