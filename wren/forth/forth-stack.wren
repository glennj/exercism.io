class ForthStack {
  construct new() {
    _list = []
  }

  toList { _list[0..-1] }

  push(elem) { _list.add(elem) }
  pop()      { _list.removeAt(-1) }
  peek()     { _list[-1] }

  need(size) {
    if (_list.count < size) {
      Fiber.abort("Stack empty")
    }
  }

  add() { 
    need(2)
    push(pop() + pop())
  }

  negate() {
    need(1)
    push(pop() * -1)
  }

  sub() { 
    need(2)
    negate()
    add()
  }

  mul() { 
    need(2)
    push(pop() * pop())
  }

  div() {
    need(2)
    var divisor = pop()
    if (divisor == 0) Fiber.abort("Division by zero")
    push((pop() / divisor).floor)
  }

  dup() {
    need(1)
    push(peek())
  }

  drop() {
    need(1)
    pop()
  }

  swap() {
    need(2)
    var a = pop()
    var b = pop()
    push(a)
    push(b)
  }

  over() {
    need(2)
    var a = pop()
    dup()
    push(a)
    swap()
  }
}
