package account

import "sync"

type Account struct {
	balance int64
	isOpen  bool
	mutex   sync.Mutex
}

func Open(amount int64) (a *Account) {
	if amount >= 0 {
		a = &Account{balance: amount, isOpen: true}
	}
	return
}

func (a *Account) Balance() (balance int64, ok bool) {
	a.mutex.Lock()
	defer a.mutex.Unlock()
	if a.isOpen {
		balance = a.balance
		ok = true
	}
	return
}

func (a *Account) Deposit(amount int64) (balance int64, ok bool) {
	a.mutex.Lock()
	defer a.mutex.Unlock()
	if a.isOpen && amount+a.balance >= 0 {
		a.balance += amount
		balance = a.balance
		ok = true
	}
	return
}

func (a *Account) Close() (cash int64, ok bool) {
	a.mutex.Lock()
	defer a.mutex.Unlock()
	if a.isOpen {
		cash = a.balance
		a.balance = 0
		a.isOpen = false
		ok = true
	}
	return
}

/* benches
 *
 * on a 1 core linode:
 * BenchmarkAccountOperations              34020756        34.05 ns/op       0 B/op       0 allocs/op
 * BenchmarkAccountOperationsParallel      36511257        35.55 ns/op       0 B/op       0 allocs/op
 *
 * on my laptop
 * BenchmarkAccountOperations-12           38972752        31.25 ns/op       0 B/op       0 allocs/op
 * BenchmarkAccountOperationsParallel-12   11818558       102.9 ns/op        0 B/op       0 allocs/op
 */
