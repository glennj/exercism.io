class BankAccount
  new: =>
    @initialize!

  initialize: =>
    @_balance = 0
    @_is_open = false

  open: =>
    assert not @_is_open, 'account already open'
    @_is_open = true

  close: =>
    assert @_is_open, 'account not open'
    @initialize!

  balance: =>
    assert @_is_open, 'account not open'
    @_balance

  deposit: (amount) =>
    assert @_is_open, 'account not open'
    assert amount > 0, 'amount must be greater than 0'
    @_balance += amount

  withdraw: (amount) =>
    assert @_is_open, 'account not open'
    assert amount <= @_balance, 'amount must be less than balance'
    assert amount > 0, 'amount must be greater than 0'
    @_balance -= amount
