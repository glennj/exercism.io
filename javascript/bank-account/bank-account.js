
export class ValueError extends Error {
  constructor() {
    super('Bank account error');
  }
}

export class BankAccount {
  constructor() {
    this._balance = 0;
    this.isOpen = false;
  }

  open() {
    if (this.isOpen)
      throw new ValueError();
    this.isOpen = true;
    return this;
  }

  close() {
    if (!this.isOpen)
      throw new ValueError();
    this.withdraw(this.balance);
    this.isOpen = false;
    return this;
  }

  deposit(amount) {
    if (!this.isOpen || amount < 0)
      throw new ValueError();
    this._balance += amount;
    return this;
  }

  withdraw(amount) {
    if (!this.isOpen || amount < 0 || amount > this.balance)
      throw new ValueError();
    this._balance -= amount;
    return this;
  }

  get balance() {
    if (!this.isOpen)
      throw new ValueError();
    return this._balance;
  }
}
