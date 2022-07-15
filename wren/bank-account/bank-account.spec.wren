import "wren-testie/testie" for Testie, Expect
import "./bank-account" for BankAccount

Testie.test("Bank account") { |do, skip|
  do.test("newly opened account has zero balance") {
    var account = BankAccount.new()
    account.open()
    Expect.value(account.balance).toEqual(0)
  }

  skip.test("can deposit money") {
    var account = BankAccount.new()
    account.open()
    account.deposit(100)
    Expect.value(account.balance).toEqual(100)
  }

  skip.test("can deposit money sequentially") {
    var account = BankAccount.new()
    account.open()
    account.deposit(100)
    account.deposit(50)
    Expect.value(account.balance).toEqual(150)
  }

  skip.test("can withdraw money") {
    var account = BankAccount.new()
    account.open()
    account.deposit(100)
    account.withdraw(50)
    Expect.value(account.balance).toEqual(50)
  }

  skip.test("can withdraw money sequentially") {
    var account = BankAccount.new()
    account.open()
    account.deposit(100)
    account.withdraw(20)
    account.withdraw(80)
    Expect.value(account.balance).toEqual(0)
  }

  skip.test("checking balance of closed account throws error") {
    var account = BankAccount.new()
    account.open()
    account.close()
    Expect.that {
      account.balance
    }.abortsWith("Bank account error")
  }

  skip.test("deposit into closed account throws error") {
    var account = BankAccount.new()
    account.open()
    account.close()
    Expect.that {
      account.deposit(50)
    }.abortsWith("Bank account error")
  }

  skip.test("withdraw from closed account throws error") {
    var account = BankAccount.new()
    account.open()
    account.close()
    Expect.that {
      account.withdraw(50)
    }.abortsWith("Bank account error")
  }

  skip.test("close already closed account throws error") {
    var account = BankAccount.new()
    Expect.that {
      account.close()
    }.abortsWith("Bank account error")
  }

  skip.test("open already opened account throws error") {
    var account = BankAccount.new()
    account.open()
    Expect.that {
      account.open()
    }.abortsWith("Bank account error")
  }

  skip.test("reopened account does not retain balance") {
    var account = BankAccount.new()
    account.open()
    account.deposit(50)
    account.close()
    account.open()
    Expect.value(account.balance).toEqual(0)
  }

  skip.test("cannot withdraw more than deposited") {
    var account = BankAccount.new()
    account.open()
    account.deposit(25)
    Expect.that {
      account.withdraw(50)
    }.abortsWith("Bank account error")
  }

  skip.test("cannot withdraw negative amount") {
    var account = BankAccount.new()
    account.open()
    account.deposit(100)
    Expect.that {
      account.withdraw(-50)
    }.abortsWith("Bank account error")
  }

  skip.test("cannot deposit negative amount") {
    var account = BankAccount.new()
    account.open()
    Expect.that {
      account.deposit(-50)
    }.abortsWith("Bank account error")
  }
}
