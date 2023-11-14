public class BankAccount {
    private enum Status { OPEN, CLOSED }

    private int balance = 0;
    private Status status = Status.CLOSED;

    synchronized void open() throws BankAccountActionInvalidException {
        if (status == Status.OPEN)
            throw new BankAccountActionInvalidException("Account already open");

        status = Status.OPEN;
    }

    synchronized void close() throws BankAccountActionInvalidException {
        if (status == Status.CLOSED)
            throw new BankAccountActionInvalidException("Account not open");

        balance = 0;
        status = Status.CLOSED;
    }

    synchronized int getBalance() throws BankAccountActionInvalidException {
        if (status == Status.CLOSED)
            throw new BankAccountActionInvalidException("Account closed");

        return balance;
    }

    synchronized void deposit(int amount) throws BankAccountActionInvalidException {
        if (status == Status.CLOSED)
            throw new BankAccountActionInvalidException("Account closed");
        if (amount < 0)
            throw new BankAccountActionInvalidException("Cannot deposit or withdraw negative amount");

        balance += amount;
    }

    synchronized void withdraw(int amount) throws BankAccountActionInvalidException {
        if (status == Status.CLOSED)
            throw new BankAccountActionInvalidException("Account closed");
        if (amount < 0)
            throw new BankAccountActionInvalidException("Cannot deposit or withdraw negative amount");
        if (balance == 0)
            throw new BankAccountActionInvalidException("Cannot withdraw money from an empty account");
        if (balance < amount)
            throw new BankAccountActionInvalidException("Cannot withdraw more money than is currently in the account");

        balance -= amount;
    }

}
