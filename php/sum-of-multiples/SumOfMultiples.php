<?php

declare(strict_types=1);

function sumOfMultiples(int $number, array $multiples): int
{
    $factors = array_fill(0, $number, false);
    foreach (array_filter($multiples, fn($m) => $m > 0) as $m) {
        for ($i = $m; $i < $number; $i += $m) {
            $factors[$i] = true;
        }
    }
    return array_sum(array_keys(array_filter($factors, fn($f) => $f)));
}
