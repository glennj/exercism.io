BankAccount = require('bank_account')

describe 'bank_account', ->

  it "Newly opened account has zero balance", ->
    acct = BankAccount!
    acct\open!
    result = acct\balance!
    assert.are.equal 0, result

  it "Single deposit", ->
    acct = BankAccount!
    acct\open!
    acct\deposit 100
    result = acct\balance!
    assert.are.equal 100, result

  it "Multiple deposits", ->
    acct = with BankAccount!
      \open!
      \deposit 100
      \deposit 50
    result = acct\balance!
    assert.are.equal 150, result

  it "Withdraw once", ->
    acct = with BankAccount!
      \open!
      \deposit 100
      \withdraw 75
    result = acct\balance!
    assert.are.equal 25, result

  it "Withdraw twice", ->
    acct = with BankAccount!
      \open!
      \deposit 100
      \withdraw 80
      \withdraw 20
    result = acct\balance!
    assert.are.equal 0, result

  it "Can do multiple operations sequentially", ->
    acct = with BankAccount!
      \open!
      \deposit 100
      \deposit 110
      \withdraw 200
      \deposit 60
      \withdraw 50
    result = acct\balance!
    assert.are.equal 20, result

  it "Cannot check balance of closed account", ->
    acct = with BankAccount!
      \open!
      \close!
    assert.has.errors acct\balance, "account not open"

  it "Cannot deposit into closed account", ->
    acct = with BankAccount!
      \open!
      \close!
    assert.has.errors (-> acct\deposit 50), "account not open"

  it "Cannot deposit into unopened account", ->
    acct = BankAccount!
    assert.has.errors (-> acct\deposit 50), "account not open"

  it "Cannot withdraw from closed account", ->
    acct = with BankAccount!
      \open!
      \close!
    assert.has.errors (-> acct\withdraw 50), "account not open"

  it "Cannot close an account that was not opened", ->
    acct = BankAccount!
    assert.has.errors acct\close, "account not open"

  it "Cannot open an already opened account", ->
    acct = with BankAccount!
      \open!
    assert.has.errors acct\open, "account already open"

  it "Reopened account does not retain balance", ->
    acct = with BankAccount!
      \open!
      \deposit 50
      \close!
      \open!
    result = acct\balance!
    assert.are.equal 0, result

  it "Cannot withdraw more than deposited", ->
    acct = with BankAccount!
      \open!
      \deposit 25
    assert.has.errors (-> acct\withdraw 50), "amount must be less than balance"

  it "Cannot withdraw negative", ->
    acct = with BankAccount!
      \open!
      \deposit 100
    assert.has.errors (-> acct\withdraw -50), "amount must be greater than 0"

  it "Cannot deposit negative", ->
    acct = with BankAccount!
      \open!
    assert.has.errors (-> acct\deposit -50), "amount must be greater than 0"
