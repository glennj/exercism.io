# Bank Account

Welcome to Bank Account on Exercism's Python Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

Simulate a bank account supporting opening/closing, withdrawals, and deposits of money.
Watch out for concurrent transactions!

A bank account can be accessed in multiple ways.
Clients can make deposits and withdrawals using the internet, mobile phones, etc.
Shops can charge against the account.

Create an account that can be accessed from multiple threads/processes (terminology depends on your programming language).

It should be possible to close an account; operations against a closed account must fail.

## Exception messages

Sometimes it is necessary to [raise an exception](https://docs.python.org/3/tutorial/errors.html#raising-exceptions). When you do this, you should always include a **meaningful error message** to indicate what the source of the error is. This makes your code more readable and helps significantly with debugging. For situations where you know that the error source will be a certain type, you can choose to raise one of the [built in error types](https://docs.python.org/3/library/exceptions.html#base-classes), but should still include a meaningful message.

This particular exercise requires that you use the [raise statement](https://docs.python.org/3/reference/simple_stmts.html#the-raise-statement) to "throw" a `ValueError` for when an account is or is not open, or the withdrawal/deposit amounts are incorrect. The tests will only pass if you both `raise` the `exception` and include a message with it.

To raise a `ValueError` with a message, write the message as an argument to the `exception` type:  


```python
# account is not open
raise ValueError('account not open')

# account is already open
raise ValueError('account already open')

# incorrect withdrawal/deposit amount
raise ValueError('amount must be greater than 0')

# withdrawal is too big
raise ValueError('amount must be less than balance')
```

## Source

### Contributed to by

- @cmccandless
- @Dog
- @tqa236
- @bethanyg
- @meatball133