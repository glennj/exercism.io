class Node
  constructor: ({@value, @next, @prev}) ->
    
class LinkedList
  constructor: ->
    @head = null
    @tail = null
    @count = 0
    
  countNodes: -> @count

  # add to head
  unshiftNode: (value) ->
    @count++
    node = new Node {value: value, next: @head}
    if @head?
      @head.prev = node
    else
      @tail = node
    @head = node

  # add to tail
  pushNode: (value) ->
    @count++
    node = new Node {value: value, prev: @tail}
    if @tail?
      @tail.next = node
    else
      @head = node
    @tail = node

  # remove from head
  shiftNode: ->
    @count--
    return if @head is null
    node = @head
    if node.next?
      @head = node.next
      @head.prev = null
    else
      @head = null
      @tail = null
    node.value

  # remove from tail
  popNode: ->
    @count--
    return if @tail is null
    node = @tail
    if node.prev?
      @tail = node.prev
      @tail.next = null
    else
      @head = null
      @tail = null
    node.value

  # remove the first node with this value
  deleteNode: (value) ->
    node = @head
    while node?
      if node.value isnt value
        node = node.next
      else
        @count--
        if node.prev?
          node.prev.next = node.next
        else
          @head = node.next
        if node.next?
          node.next.prev = node.prev
        else
          @tail = node.prev
        break


module.exports = LinkedList

