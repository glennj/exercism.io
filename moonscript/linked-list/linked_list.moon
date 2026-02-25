class LinkedList
  -- nothing to be done in a constructor, so don't make one

  push: (value) =>
    node = {:value}
    if @tail
      node.prev = @tail
      @tail.next = node
    else
      @head = node
    @tail = node

  unshift: (value) =>
    node = {:value}
    if @head
      node.next = @head
      @head.prev = node
    else
      @tail = node
    @head = node

  pop: =>
    local value
    node = @tail
    if node
      value = node.value
      if node.prev
        node.prev.next = nil
        @tail = node.prev
      else
        @tail = nil
        @head = nil
    value

  shift: =>
    local value
    node = @head
    if node
      value = node.value
      if node.next
        node.next.prev = nil
        @head = node.next
      else
        @tail = nil
        @head = nil
    value

  count: =>
    n = 0
    node = @head
    while node
      n += 1
      node = node.next
    n

  delete: (value) =>
    node = @head
    while node
      if node.value != value
        node = node.next
        continue

      -- unlink this node
      if node.next
        node.next.prev = node.prev
      else
        @tail = node.prev
      if node.prev
        node.prev.next = node.next
      else
        @head = node.next
      return
