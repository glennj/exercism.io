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
                raise ValueError('account not open')
            return self.balance

    def open(self):
        with self.lock:
            if self.is_open:
                raise ValueError('account already open')
            self.is_open = True
            self.balance = 0

    def deposit(self, amount):
        if amount < 0:
            raise ValueError('amount must be greater than 0')
        with self.lock:
            if not self.is_open:
                raise ValueError('account not open')
            self.balance += amount

    def withdraw(self, amount):
        if amount < 0:
            raise ValueError('amount must be greater than 0')
        with self.lock:
            if not self.is_open:
                raise ValueError('account not open')
            if amount > self.balance:
                raise ValueError('amount must be less than balance')
            self.balance -= amount

    def close(self):
        with self.lock:
            if not self.is_open:
                raise ValueError('account not open')
            self.is_open = False
