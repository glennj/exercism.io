class BankAccount {
  static initialize {
    __OPEN = 1
    __CLOSED = 0
  }

  construct new() {
    _state = __CLOSED
  }

  assert(func) {
    if (!func.call()) {
      Fiber.abort("Bank account error")
    }
  }

  open() {
    assert {_state != __OPEN}
    _state = __OPEN
    _balance = 0
  }

  close() {
    assert {_state == __OPEN}
    _state = __CLOSED
  }

  deposit(amount) {
    assert {_state == __OPEN}
    assert {amount >= 0}
    _balance = _balance + amount
  }

  withdraw(amount) {
    assert {_state == __OPEN}
    assert {amount <= _balance}
    assert {amount >= 0}
    _balance = _balance - amount
  }

  balance {
    assert {_state == __OPEN}
    return _balance
  }
}

BankAccount.initialize
