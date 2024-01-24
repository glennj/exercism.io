<?php

class LuckyNumbers
{
    public function sumUp(array $digitsOfNumber1, array $digitsOfNumber2): int
    {
        $a = (int) implode('', $digitsOfNumber1);
        $b = (int) implode('', $digitsOfNumber2);
        return $a + $b;
    }

    public function isPalindrome(int $number): bool
    {
        return $number == (int) strrev($number);
    }

    public function validate(string $input): string
    {
        if (strlen($input) == 0) {
            return 'Required field';
        } elseif ((int) $input <= 0) {
            return 'Must be a whole number larger than 0';
        }

        return '';
    }
}
