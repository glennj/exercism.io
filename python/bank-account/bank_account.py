import threading


'''
Syncronize every method. For example, an account is
in the process of withdrawing in one thread, and is
being closed in another thread.
'''

class BankAccount(object):
    def __init__(self):
        self.is_open = False
        self.lock = threading.RLock()

    def get_balance(self):
        with self.lock:
            if not self.is_open:
                raise ValueError('Account is not open.')
            return self.balance

    def open(self):
        with self.lock:
            if self.is_open:
                raise ValueError('Account is already open.')
            self.is_open = True
            self.balance = 0

    def deposit(self, amount):
        if amount < 0:
            raise ValueError('Invalid deposit.')
        with self.lock:
            if not self.is_open:
                raise ValueError('Account is not open.')
            self.balance += amount

    def withdraw(self, amount):
        if amount < 0:
            raise ValueError('Invalid withdrawal.')
        with self.lock:
            if not self.is_open:
                raise ValueError('Account is not open.')
            if amount > self.balance:
                raise ValueError('Insufficient funds.')
            self.balance -= amount

    def close(self):
        with self.lock:
            if not self.is_open:
                raise ValueError('Account is already not open.')
            self.is_open = False
