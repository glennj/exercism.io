/* It would be nice to have the Stack inherit from List, but:
 *
 *    "Note that you should not create classes that inherit
 *    from the built-in types (Bool, Num, String, Range,
 *    List). The built-in types expect their internal bit
 *    representation to be very specific and get horribly
 *    confused when you invoke one of the inherited
 *    built-in methods on the derived type."
 *      -- https://wren.io/classes.html#inheritance
 *
 * So we have has-a not is-a
 */

class Stack {
  construct new() {
    _list = []
  }

  isEmpty {
    return _list.isEmpty
  }

  push(elem) {
    return _list.add(elem)
  }

  pop() {
    if (isEmpty) {
      Fiber.abort("Stack is empty")
    }
    return _list.removeAt(-1)
  }
}
