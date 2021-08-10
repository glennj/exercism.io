class ForthStack {
  construct new() {
    _list = []
  }

  toList { _list }

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

  sub() { 
    need(2)
    push(pop() * -1 + pop())
  }

  mul() { 
    need(2)
    push(pop() * pop())
  }

  div() {
    need(2)
    var a = pop()
    if (a == 0) Fiber.abort("Division by zero")
    push((pop() / a).floor)
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
