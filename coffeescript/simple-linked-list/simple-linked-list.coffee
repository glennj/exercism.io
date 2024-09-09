class Element
  constructor: (@value) ->
    @next = null

class LinkedList
  constructor: (values = []) ->
    @head = null
    @count = 0
    for v in values
      @add new Element v

  length: -> @count

  add: (element) ->
    element.next = @head
    @head = element
    @count += 1

  toArray: ->
    # returns values in the reverse order they were inserted (LIFO)
    values = []
    elem = @head
    while elem?
      values.push elem.value
      elem = elem.next
    values

  reverse: -> new LinkedList @toArray()

module.exports = {Element, LinkedList}
