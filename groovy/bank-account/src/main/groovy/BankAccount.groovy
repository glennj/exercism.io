import groovy.transform.Synchronized

class BankAccount {
    private boolean isOpen = false
    private int balance = 0

    @Synchronized
    void open() {
        isOpen = true
    }

    @Synchronized
    void close() {
        isOpen = false
    }

    @Synchronized
    def notClosed() { 
        if (!isOpen) throw new Exception("Account is closed")
    }

    @Synchronized
    def validAmount(int amount) {
        if (amount < 0) throw new Exception("Invalid amount")
    }

    @Synchronized
    void deposit(int amount) {
        notClosed()
        validAmount(amount)
        balance += amount
    }

    @Synchronized
    void withdraw(int amount) {
        notClosed()
        validAmount(amount)
        if (amount > balance) throw new Exception("Insufficient funds")
        balance -= amount
    }

    @Synchronized
    int getBalance() {
        notClosed()
        balance
    }
}
