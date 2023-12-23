use context essentials2020

include file("bank-account.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun open-account-zero-balance():
  check "Newly opened account has zero balance":
    account().open().get-balance() is 0
  end
end

fun single-deposit():
  check "Single deposit":
    account().open().deposit(100).get-balance() is 100
 end
end

fun multiple-deposits():
  check "Multiple deposits":
    account().open().deposit(100).deposit(50).get-balance() is 150
  end
end

fun withdraw-once():
  check "Withdraw once":
    account().open().deposit(100).withdraw(75).get-balance() is 25
  end
end

fun withdraw-twice():
  check "Withdraw twice":
    account().open().deposit(100).withdraw(80).withdraw(20).get-balance() is 0
  end
end

fun multiple-operations():
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
end

fun no-balance-for-closed-account():
  check "Cannot check balance of closed account":
    account().open().close().get-balance() raises "account not open"
  end
end

fun no-deposit-for-closed-account():
  check "Cannot deposit into closed account":
    account().open().close().deposit(50) raises "account not open"
  end
end

fun no-deposit-for-unopened-account():
  check "Cannot deposit into unopened account":
    account().deposit(50) raises "account not open"
  end
end

fun no-withdraw-for-closed-account():
  check "Cannot withdraw from closed account":
    account().open().close().withdraw(50) raises "account not open"
  end
end

fun no-close-unopened-account():
  check "Cannot close an account that was not opened":
    account().close() raises "account not open"
  end
end

fun no-open-already-opened-account():
  check "Cannot open an already opened account":
    account().open().open() raises "account already open"
  end
end

fun reopened-account-does-not-retain-balance():
  check "Reopened account does not retain balance":
    account().open().deposit(50).close().open().get-balance() is 0
  end
end

fun no-withdraw-more-than-deposited():
  check "Cannot withdraw more than deposited":
    account().open().deposit(25).withdraw(50) raises "amount must be less than balance"
  end
end

fun no-withdraw-negative-amount():
  check "Cannot withdraw negative":
    account().open().deposit(100).withdraw(-50) raises "amount must be greater than 0"
  end
end

fun no-deposit-negative-amount():
  check "Cannot deposit negative":
    account().open().deposit(-50) raises "amount must be greater than 0"
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, false) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(open-account-zero-balance, true),
  test(single-deposit, true),
  test(multiple-deposits, true),
  test(withdraw-once, true),
  test(withdraw-twice, true),
  test(multiple-operations, true),
  test(no-balance-for-closed-account, true),
  test(no-deposit-for-closed-account, true),
  test(no-deposit-for-unopened-account, true),
  test(no-withdraw-for-closed-account, true),
  test(no-close-unopened-account, true),
  test(no-open-already-opened-account, true),
  test(reopened-account-does-not-retain-balance, true),
  test(no-withdraw-more-than-deposited, true),
  test(no-withdraw-negative-amount, true),
  test(no-deposit-negative-amount, true)
].each(lam(t): when t.active: t.run() end end)
