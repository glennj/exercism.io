local BankAccount = {}
BankAccount.__index = BankAccount

local AccountStatus = { Open = 1, Closed = 2 }

function BankAccount:new()
    local acct = {}
    setmetatable(acct, self)

    acct._balance = 0
    acct._status = AccountStatus.Closed
    return acct
end

function BankAccount:is_open()
    return self._status == AccountStatus.Open
end

function BankAccount:open()
    assert(not self:is_open())
    self._status = AccountStatus.Open
end

function BankAccount:close()
    assert(self:is_open())
    self._status = AccountStatus.Closed
    -- return the customer's money
    local withdrawal = self._balance
    self._balance = 0
    return withdrawal
end

function BankAccount:balance()
    assert(self:is_open())
    return self._balance
end

function BankAccount:deposit(amount)
    assert(self:is_open())
    assert(amount > 0)
    self._balance = self._balance + amount
end

function BankAccount:withdraw(amount)
    assert(self:is_open())
    assert(amount > 0)
    assert(amount <= self._balance)
    self._balance = self._balance - amount
end

return BankAccount
