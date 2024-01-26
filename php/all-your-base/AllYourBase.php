<?php

declare(strict_types=1);
require_once 'extra_array_funcs.php';

function rebase(int $from_base, array $digits, int $to_base): array|null
{
    // validation
    if ($from_base < 2
        || $to_base < 2
        || empty($digits)
        || $digits[0] == 0
        || array_all(fn ($digit) => $digit == 0, $digits)
        || array_any(fn ($digit) => $digit < 0 || $digit >= $from_base, $digits)
    ) {
        return null;
    }

    // recursive anonymous function to create the result array of digits
    $to_digits = function (int $n, array $digits = []) use ($to_base, &$to_digits) {
        if ($n == 0) {
            return $digits;
        }
        return $to_digits(intdiv($n, $to_base), [$n % $to_base, ...$digits]);
    };

    $decimal_value = array_reduce(
        $digits,
        fn($acc, $digit) => $from_base * $acc + $digit,
        0
    );

    return $to_digits($decimal_value);
}
