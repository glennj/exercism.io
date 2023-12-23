use context essentials2020

provide-types *

data Account:
  | account() 
  | open-account(balance :: Number)

sharing:
  method open(self):
    ask:
      | is-open-account(self) then: raise("account already open")
      | otherwise: open-account(0)
    end
  end,

  method close(self):
    ask:
      | is-account(self) then: raise("account not open")
      | otherwise: account()
    end
  end,

  method deposit(self, amount):
    ask:
      | is-account(self) then: raise("account not open")
      | amount < 0 then: raise("amount must be greater than 0")
      | otherwise: open-account(self.balance + amount)
    end
  end,

  method withdraw(self, amount):
    ask:
      | is-account(self) then: raise("account not open")
      | amount < 0 then: raise("amount must be greater than 0")
      | amount > self.balance then: raise("amount must be less than balance")
      | otherwise: open-account(self.balance - amount)
    end
  end,

  method get-balance(self):
    ask:
      | is-account(self) then: raise("account not open")
      | otherwise: self.balance
    end
  end
end
