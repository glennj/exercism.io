class BankAccount
  @@bank_profits = 0

  def initialize
    @is_open = false
    @balance = 0
  end

  def balance
    assert @is_open, "You can't check the balance of a closed account"
    @balance
  end

  def open
    assert !@is_open, "You can't open an already open account"
    @is_open = true
  end

  def close
    assert @is_open, "You can't close an already closed account"
    @is_open = false
    @@bank_profits += @balance  # @mood = :cynicism
    @balance = 0
  end

  def deposit(amount)
    assert @is_open, "You can't deposit money into a closed account"
    assert amount >= 0, "You can't deposit a negative amount"
    @balance += amount
  end

  def withdraw(amount)
    assert @is_open, "You can't withdraw money into a closed account"
    assert amount <= @balance, "You can't withdraw more than you have"
    assert amount >= 0, "You can't withdraw a negative amount"
    @balance -= amount
  end

  private def assert(condition, message)
    raise ArgumentError.new(message) if not condition
  end
end
