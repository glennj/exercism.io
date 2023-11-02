class Bucket {
  construct new(name, size) {
    _name = name
    _size = size
    _amt = 0
  }

  name { _name }
  size { _size }
  amount { _amt }
  available { size - amount }

  isFull { amount == size }
  isEmpty { amount == 0 }

  fill() { _amt = _size }
  empty() { _amt = 0 }
  add(quantity) { _amt = _amt + quantity }

  pourInto(other) {
    var quantity = amount.min(other.available)
    other.add(quantity)
    this.add(-quantity)
  }
}
