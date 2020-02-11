class BankAccount {

    private var isOpen = true

    var balance = 0L
        get() {
            synchronized(this) {
                check(isOpen)
                return field
            }
        }
        private set

    fun adjustBalance(amount: Long) {
        synchronized(this) {
            check(isOpen)
            balance += amount
        }
    }

    fun close() {
        synchronized(this) {
            isOpen = false
        }
    }
}

// note to self: check() throws IllegalStateException
