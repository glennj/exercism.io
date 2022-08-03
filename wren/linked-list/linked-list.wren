class LinkedList {
  construct new() {
    _value = null
    _next = null
    _prev = null
  }

  value {_value}
  value=(val) {_value = val}

  next {_next}
  next=(node) {_next = node}

  prev {_prev}
  prev=(node) {_prev = node}

  /* unlike the bonus test, I chose to iterate, not over the _values_,
   * but over the _nodes_
   *
   *    for (node in list) {
   *      result.add(node.value / 2)
   *    }
   */
  iterate(iter) {
    if (iter == null) {
      if (value == null) return false
      return this
    }
    if (iter.next == null) return false
    return iter.next
  }
  iteratorValue(iter) {
    return iter
  }

  count {
    var count = 0
    for (node in this) {
      count = count + 1
    }
    return count
  }

  // walk to the tail
  tail {
    // "node" is local to the loop, so we need a var with a different scope
    var t
    for (node in this) {
      t = node
    }
    return t
  }

  push(val) {
    if (value == null) {
      // initializing a new empty list
      value = val
      return
    }

    var t = tail
    var node = type.new()
    node.value = val
    t.next = node
    node.prev = t
  }

  unshift(val) {
    if (value == null) {
      // initializing a new empty list
      value = val
      return
    }

    // create a new node and populate it with the current head info
    var node = type.new()
    node.value = this.value
    node.next = this.next
    if (node.next != null) node.next.prev = node

    // and "this" gets the new value and links to the new node (the "old head")
    value = val
    next = node
    node.prev = this
  }

  pop() {
    var t = tail
    var val = t.value
    if (t.prev == null) {
      // this is also the head
      t.value = null
    } else {
      t.prev.next = null
    }
    return val
  }

  shift() {
    var val = value
    if (next == null) {
      // this is also the tail
      value = null
    } else {
      var node = next
      value = node.value
      next = node.next
      if (node.next != null) node.next.prev = this
    }
    return val
  }

  delete(searchValue) {
    // special case: delete the head
    if (value == searchValue) {
      shift()
      return
    }

    var del
    for (node in this) {
      del = node
      if (del.value == searchValue) break
    }
    if (del.value == searchValue) {
      if (del.next != null) del.next.prev = del.prev
      if (del.prev != null) del.prev.next = del.next
    }
  }
}
