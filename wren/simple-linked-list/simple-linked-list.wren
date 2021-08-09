class Element {
  construct new(value) {
    _val = value
    _next = null
  }
  value { _val }
  next  { _next }
  next=(element) { _next = element }
}


/*
 * This linked list implementation acts like a stack:
 * the most recently inserted element sits at the head
 * of the list.
 */
class LinkedList {
  construct new() {
    init()
  }
  construct new(values) {
    init()
    values.each {|value| add(Element.new(value))}
  }

  init() {
    _head = null
    _count = 0
  }

  head  { _head }
  count { _count }

  add(element) {
    if (element.type != Element) {
      Fiber.abort("Only add Elements to the LinkedList")
    }
    if (_head != null) element.next = _head
    _head = element
    _count = _count + 1
    return this
  }

  // this returns the values in _reverse insertion order_
  toList {
    var list = []
    for (value in this) {
      list.add(value)
    }
    return list
  }

  reverse() { type.new(toList) }

  // the iterator protocol
  iterate(element) {
    if (element == null) {      // beginning of the iteration
      return _head
    }
    if (element.next == null) { // end of the line
      return false
    }
    return element.next
  }
  iteratorValue(element) { element.value }
}
