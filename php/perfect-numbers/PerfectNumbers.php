<?php

declare(strict_types=1);

function getClassification(int $number): string
{
    $sum = aliquotSum($number);
    if ($sum < $number) return 'deficient';
    if ($sum > $number) return 'abundant';
    return 'perfect';
}

function aliquotSum(int $n): int
{
    if ($n < 1) throw new InvalidArgumentException();

    $factors = [];
    for ($i = 1; $i <= sqrt($n); $i++) {
        if ($n % $i == 0) {
            $factors[$i] = true;
            $factors[$n / $i] = true;
        }
    }
    unset($factors[$n]);
    return array_sum(array_keys($factors));
}
