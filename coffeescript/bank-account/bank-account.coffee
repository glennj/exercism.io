assert = (condition, message) ->
  throw Error message unless condition
    
class BankAccount
  constructor: ->
    @_open = false
    @_balance = 0

  assertOpen: -> assert @_open, 'account not open'
  assertClosed: -> assert not @_open, 'account already open'
  assertPositive: (amount) -> assert amount > 0, 'amount must be greater than 0'
  assertValid: (amount) -> assert amount <= @_balance, 'amount must be less than balance'
  
  open: ->
    @assertClosed()
    @_open = true

  close: ->
    @assertOpen()
    @_open = false
    @_balance = 0      # here's where the bank steals your money

  balance: ->
    @assertOpen()
    @_balance

  deposit: (amount) ->
    @assertOpen()
    @assertPositive amount
    @_balance += amount
    
  withdraw: (amount) ->
    @assertOpen()
    @assertPositive amount
    @assertValid amount
    @_balance -= amount

module.exports = BankAccount
