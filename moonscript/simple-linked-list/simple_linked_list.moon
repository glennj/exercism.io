class SimpleLinkedList
  new: (items) =>
    @push item for item in *(items or {})

  push: (item) =>
    @head = {value: item, next: @head}

  pop: =>
    item = @peek!
    @head = @head.next
    item

  peek: =>
    assert @head, 'list is empty'
    @head.value

  reduce: (acc, func) =>
    node = @head
    while node
      acc = func acc, node.value
      node = node.next
    acc
    
  count: => @reduce 0, (c, _) -> c + 1

  toList: => @reduce {}, (list, item) -> {item, table.unpack list}

  -- this returns a new instance
  reversed: =>
    @reduce @@!, (rev, item) -> 
      with rev
        \push item

  -- this reverses in-place
  reverse: => @head = @reversed!.head
