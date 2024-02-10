<?php

declare(strict_types=1);

function isValid(string $number): bool
{
    $digit_value = [
        false => [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
        true  => [0, 2, 4, 6, 8, 1, 3, 5, 7, 9]
    ];

    $digits = preg_replace('/\s/', '', $number);
    if (strlen($digits) < 2 || !ctype_digit($digits)) {
        return false;
    }

    $sum = array_reduce(
        array_reverse(str_split($digits)),
        fn ($a, $d) => [$a[0] + $digit_value[$a[1]][$d], !$a[1]],
        [0, false]
    )[0];

    return $sum % 10 == 0;
}
