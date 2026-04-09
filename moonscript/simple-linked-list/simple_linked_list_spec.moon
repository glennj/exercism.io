SimpleLinkedList = require 'simple_linked_list'

describe 'simple-linked-list', ->
  describe 'count', ->
    it 'Empty list has length of zero', ->
      list = SimpleLinkedList {}
      result = list\count!
      assert.are.equal 0, result

    it 'Singleton list has length of one', ->
      list = SimpleLinkedList {1}
      result = list\count!
      assert.are.equal 1, result

    it 'Non-empty list has correct length', ->
      list = SimpleLinkedList {1, 2, 3}
      result = list\count!
      assert.are.equal 3, result

  describe 'pop', ->
    it 'Pop from empty list is an error', ->
      list = SimpleLinkedList {}
      f = -> list\pop!
      assert.has.error f, 'list is empty'

    it 'Can pop from singleton list', ->
      list = SimpleLinkedList {1}
      result = list\pop!
      assert.are.equal 1, result

    it 'Can pop from non-empty list', ->
      list = SimpleLinkedList {1, 2}
      result = list\pop!
      assert.are.equal 2, result

    it 'Can pop multiple items', ->
      list = SimpleLinkedList {1, 2}
      result = list\pop!
      assert.are.equal 2, result
      result = list\pop!
      assert.are.equal 1, result

    it 'Pop updates the count', ->
      list = SimpleLinkedList {1, 2}
      result = list\count!
      assert.are.equal 2, result
      result = list\pop!
      assert.are.equal 2, result
      result = list\count!
      assert.are.equal 1, result
      result = list\pop!
      assert.are.equal 1, result
      result = list\count!
      assert.are.equal 0, result

  describe 'push', ->
    it 'Can push to an empty list', ->
      list = SimpleLinkedList!
      list\push 1

    it 'Can push to a non-empty list', ->
      list = SimpleLinkedList {1, 2}
      list\push 3

    it 'Push updates count', ->
      list = SimpleLinkedList {1, 2}
      list\push 3
      result = list\count!
      assert.are.equal 3, result

    it 'Push and pop', ->
      list = SimpleLinkedList {}
      list\push 1
      list\push 2
      result = list\pop!
      assert.are.equal 2, result
      list\push 3
      result = list\count!
      assert.are.equal 2, result
      result = list\pop!
      assert.are.equal 3, result
      result = list\pop!
      assert.are.equal 1, result
      result = list\count!
      assert.are.equal 0, result

  describe 'peek', ->
    it 'Peek on empty list is an error', ->
      list = SimpleLinkedList {}
      f = -> list\peek!
      assert.has.error f, 'list is empty'

    it 'Can peek on singleton list', ->
      list = SimpleLinkedList {1}
      result = list\peek!
      assert.are.equal 1, result

    it 'Can peek on non-empty list', ->
      list = SimpleLinkedList {1, 2}
      result = list\peek!
      assert.are.equal 2, result

    it 'Peek does not change the count', ->
      list = SimpleLinkedList {1, 2}
      result = list\peek!
      assert.are.equal 2, result
      result = list\count!
      assert.are.equal 2, result

    it 'Can peek after a pop and push', ->
      list = SimpleLinkedList {}
      list\push 1
      list\push 2
      result = list\peek!
      assert.are.equal 2, result
      result = list\pop!
      assert.are.equal 2, result
      result = list\peek!
      assert.are.equal 1, result
      list\push 3
      result = list\peek!
      assert.are.equal 3, result


  describe 'toList FIFO', ->
    it 'Empty linked list to list is empty', ->
      list = SimpleLinkedList {}
      result = list\toList!
      assert.are.same {}, result

    it 'To list with multiple values', ->
      list = SimpleLinkedList {1, 2, 3}
      result = list\toList!
      assert.are.same {1, 2, 3}, result

    it 'To list after a pop', ->
      list = SimpleLinkedList {}
      list\push 1
      list\push 2
      list\push 3
      result = list\pop!
      assert.are.equal 3, result
      list\push 4
      result = list\toList!
      assert.are.same {1, 2, 4}, result

  describe 'reverse', ->
    it 'Reversed empty list has same values', ->
      list = SimpleLinkedList {}
      list\reverse!
      result = list\toList!
      assert.are.same {}, result

    it 'Reversed singleton list is same list', ->
      list = SimpleLinkedList {1}
      list\reverse!
      result = list\toList!
      assert.are.same {1}, result

    it 'Reversed non-empty list is reversed', ->
      list = SimpleLinkedList {1, 2, 3}
      list\reverse!
      result = list\count!
      assert.are.equal 3, result
      result = list\pop!
      assert.are.equal 1, result
      result = list\pop!
      assert.are.equal 2, result
      result = list\pop!
      assert.are.equal 3, result

    it 'Double reverse', ->
      list = SimpleLinkedList {1, 2, 3}
      list\reverse!
      list\reverse!
      result = list\pop!
      assert.are.equal 3, result
      result = list\pop!
      assert.are.equal 2, result
      result = list\pop!
      assert.are.equal 1, result
