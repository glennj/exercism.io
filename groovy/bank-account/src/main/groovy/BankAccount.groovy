import groovy.transform.Synchronized

class BankAccount {
    private boolean isOpen = false
    private int balance = 0

    @Synchronized
    void open() {
        assertNotOpen()
        isOpen = true
    }

    @Synchronized
    void close() {
        assertIsOpen()
        isOpen = false
        balance = 0
    }

    @Synchronized
    void deposit(int amount) {
        assertIsOpen()
        assertValidAmount(amount)
        balance += amount
    }

    @Synchronized
    void withdraw(int amount) {
        assertIsOpen()
        assertValidAmount(amount)
        assertSufficientFunds(amount)
        balance -= amount
    }

    @Synchronized
    int getBalance() {
        assertIsOpen()
        balance
    }

    // assertions
    @Synchronized
    def assert_(condition, msg) {
        if (!condition) throw new Exception(msg)
    }
    @Synchronized
    def assertIsOpen() {
        assert_(isOpen, "Account is not open")
    }
    @Synchronized
    def assertNotOpen() {
        assert_(!isOpen, "Account is already open")
    }
    @Synchronized
    def assertValidAmount(int amount) {
        assert_(amount >= 0, "Invalid amount")
    }
    @Synchronized
    def assertSufficientFunds(int amount) {
        assert_(amount <= balance, "Insufficient funds")
    }
}
