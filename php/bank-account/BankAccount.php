<?php

declare(strict_types=1);

class BankAccount
{
    private int $_balance;
    private bool $_open;

    public function __construct()
    {
        $this->init();
    }

    private function init()
    {
        $this->_balance = 0;
        $this->_open = false;
    }

    #############################################################
    # assertions
    private function assertOpen()
    {
        if (!$this->_open) throw new Exception('account not open');
    }

    private function assertClosed()
    {
        if ($this->_open) throw new Exception('account already open');
    }

    private function assertAmountPositive(int $amount)
    {
        if ($amount <= 0) throw new InvalidArgumentException('amount must be greater than 0');
    }

    private function assertAmountValid(int $amount)
    {
        if ($amount > $this->_balance) throw new InvalidArgumentException('amount must be less than balance');
    }

    #############################################################
    public function open()
    {
        $this->assertClosed();
        $this->_open = true;
    }

    public function close()
    {
        $this->assertOpen();
        $this->init();
    }

    public function balance(): int
    {
        $this->assertOpen();
        return $this->_balance;
    }

    public function deposit(int $amt)
    {
        $this->assertOpen();
        $this->assertAmountPositive($amt);
        $this->_balance += $amt;
    }

    public function withdraw(int $amt)
    {
        $this->assertOpen();
        $this->assertAmountPositive($amt);
        $this->assertAmountValid($amt);
        $this->_balance -= $amt;
    }
}
