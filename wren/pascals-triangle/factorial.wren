// a memo-izing factorial calculator
class Factorial {
  construct new() {
    _fact = [1, 1]
  }

  factorial(n) {
    if (n >= _fact.count) {
      for (i in _fact.count..n) {
        _fact.add(_fact[-1] * i)
      }
    }
    return _fact[n]
  }

  nChooseK(n, k) {
    return factorial(n) / (factorial(k) * factorial(n - k))
  }
}
