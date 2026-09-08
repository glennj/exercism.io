use context starter2024

include file("bank-account.arr")

check "Newly opened account has zero balance":
  account().open().get-balance() is 0
end

check "Single deposit":
  account().open().deposit(100).get-balance() is 100
end

check "Multiple deposits":
  account().open().deposit(100).deposit(50).get-balance() is 150
end

check "Withdraw once":
  account().open().deposit(100).withdraw(75).get-balance() is 25
end

check "Withdraw twice":
  account().open().deposit(100).withdraw(80).withdraw(20).get-balance() is 0
end

check "Can do multiple operations sequentially":
  account()
  ^ _.open()
  ^ _.deposit(100)
  ^ _.deposit(110)
  ^ _.withdraw(200)
  ^ _.deposit(60)
  ^ _.withdraw(50)
  ^ _.get-balance() is 20
end

check "Cannot check balance of closed account":
  account().open().close().get-balance() raises "account not open"
end

check "Cannot deposit into closed account":
  account().open().close().deposit(50) raises "account not open"
end

check "Cannot deposit into unopened account":
  account().deposit(50) raises "account not open"
end

check "Cannot withdraw from closed account":
  account().open().close().withdraw(50) raises "account not open"
end

check "Cannot close an account that was not opened":
  account().close() raises "account not open"
end

check "Cannot open an already opened account":
  account().open().open() raises "account already open"
end

check "Reopened account does not retain balance":
  account().open().deposit(50).close().open().get-balance() is 0
end

check "Cannot withdraw more than deposited":
  account().open().deposit(25).withdraw(50) raises "amount must be less than balance"
end

check "Cannot withdraw negative":
  account().open().deposit(100).withdraw(-50) raises "amount must be greater than 0"
end

check "Cannot deposit negative":
  account().open().deposit(-50) raises "amount must be greater than 0"
end
