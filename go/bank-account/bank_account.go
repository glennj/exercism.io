package account

type Account struct {
	balance int64
	open    bool
}

func Open(amount int64) *Account {
	if amount < 0 {
		return nil
	}
	return &Account{balance: amount, open: true}
}

func (a *Account) Balance() (int64, bool) {
	if !a.open {
		return 0, false
	}
	return a.balance, true
}

func (a *Account) Deposit(amount int64) (int64, bool) {
	if !a.open || amount+a.balance < 0 {
		return 0, false
	}
	a.balance += amount
	return a.balance, true
}

func (a *Account) Close() (int64, bool) {
	if !a.open {
		return 0, false
	}
	cash := a.balance
	a.balance = 0
	a.open = false
	return cash, true
}
