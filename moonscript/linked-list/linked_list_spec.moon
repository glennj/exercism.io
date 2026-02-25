LinkedList = require 'linked_list'

describe 'linked-list', ->
  it 'pop gets element from the list', ->
    list = LinkedList!
    list\push 7
    assert.are.equal 7, list\pop!

  it 'push/pop respectively add/remove at the end of the list', ->
    list = LinkedList!
    list\push 11
    list\push 13
    assert.are.equal 13, list\pop!
    assert.are.equal 11, list\pop!

  it 'shift gets an element from the list', ->
    list = LinkedList!
    list\push 17
    assert.are.equal 17, list\shift!

  it 'shift gets first element from the list', ->
    list = LinkedList!
    list\push 23
    list\push 5
    assert.are.equal 23, list\shift!
    assert.are.equal 5, list\shift!

  it 'unshift adds element at start of the list', ->
    list = LinkedList!
    list\unshift 23
    list\unshift 5
    assert.are.equal 5, list\shift!
    assert.are.equal 23, list\shift!

  it 'pop, push, shift, and unshift can be used in any order', ->
    list = LinkedList!
    list\push 1
    list\push 2
    assert.are.equal 2, list\pop!
    list\push 3
    assert.are.equal 1, list\shift!
    list\unshift 4
    list\push 5
    assert.are.equal 4, list\shift!
    assert.are.equal 5, list\pop!
    assert.are.equal 3, list\shift!

  it 'count an empty list', ->
    list = LinkedList!
    assert.are.equal 0, list\count!

  it 'count a list with items', ->
    list = LinkedList!
    list\push 37
    list\push 1
    assert.are.equal 2, list\count!

  it 'count is correct after mutation', ->
    list = LinkedList!
    list\push 31
    assert.are.equal 1, list\count!
    list\unshift 43
    assert.are.equal 2, list\count!
    list\shift!
    assert.are.equal 1, list\count!
    list\pop!
    assert.are.equal 0, list\count!

  it "popping to empty doesn't break the list", ->
    list = LinkedList!
    list\push 41
    list\push 59
    list\pop!
    list\pop!
    list\push 47
    assert.are.equal 1, list\count!
    assert.are.equal 47, list\pop!

  it "shifting to empty doesn't break the list", ->
    list = LinkedList!
    list\push 41
    list\push 59
    list\shift!
    list\shift!
    list\push 47
    assert.are.equal 1, list\count!
    assert.are.equal 47, list\shift!

  it 'deletes the only element', ->
    list = LinkedList!
    list\push 61
    list\delete 61
    assert.are.equal 0, list\count!

  it 'deletes the element with the specified value from the list', ->
    list = LinkedList!
    list\push 71
    list\push 83
    list\push 79
    list\delete 83
    assert.are.equal 2, list\count!
    assert.are.equal 79, list\pop!
    assert.are.equal 71, list\shift!

  it 'deletes the element with the specified value from the list, re-assigns tail', ->
    list = LinkedList!
    list\push 71
    list\push 83
    list\push 79
    list\delete 83
    assert.are.equal 2, list\count!
    assert.are.equal 79, list\pop!
    assert.are.equal 71, list\pop!

  it 'deletes the element with the specified value from the list, re-assigns head', ->
    list = LinkedList!
    list\push 71
    list\push 83
    list\push 79
    list\delete 83
    assert.are.equal 2, list\count!
    assert.are.equal 71, list\shift!
    assert.are.equal 79, list\shift!

  it 'deletes the first of two elements', ->
    list = LinkedList!
    list\push 97
    list\push 101
    list\delete 97
    assert.are.equal 1, list\count!
    assert.are.equal 101, list\pop!

  it 'deletes the second of two elements', ->
    list = LinkedList!
    list\push 97
    list\push 101
    list\delete 101
    assert.are.equal 1, list\count!
    assert.are.equal 97, list\pop!

  it 'delete does not modify the list if the element is not found', ->
    list = LinkedList!
    list\push 89
    list\delete 103
    assert.are.equal 1, list\count!

  it 'deletes only the first occurrence', ->
    list = LinkedList!
    list\push 73
    list\push 9
    list\push 9
    list\push 107
    list\delete 9
    assert.are.equal 3, list\count!
    assert.are.equal 107, list\pop!
    assert.are.equal 9, list\pop!
    assert.are.equal 73, list\pop!
