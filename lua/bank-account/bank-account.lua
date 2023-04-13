local BankAccount = {}
BankAccount.__index = BankAccount

local AccountStatus = { Open = true, Closed = false }

function BankAccount:new()
    local acct = {}
    setmetatable(acct, self)

    acct._balance = 0
    acct._status = AccountStatus.Open
    return acct
end

function BankAccount:balance()
    return self._balance
end

function BankAccount:deposit(amount)
    assert(self._status)
    assert(amount > 0)
    self._balance = self._balance + amount
end

function BankAccount:withdraw(amount)
    assert(self._status)
    assert(amount <= self._balance)
    assert(amount > 0)
    self._balance = self._balance - amount
end

function BankAccount:close()
    self._status = AccountStatus.Closed
end

function BankAccount:status()
    for name, value in pairs(AccountStatus) do
        if self._status == value then
            return name
        end
    end
end

return BankAccount
