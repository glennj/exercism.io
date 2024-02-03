<?php

declare(strict_types=1);

function prime(int $number): int|bool
{
    if ($number < 1) return false;

    $primes = [1 => 2, 2 => 3];

    $is_prime = function(int $n) use (&$primes) {
        foreach ($primes as $p) {
            if ($n % $p == 0) return false;
            if ($p * $p > $n) break;
        }
        return true;
    };

    $add_next_prime = function() use (&$primes, $is_prime) {
        $p = end($primes) + 2;
        while (! $is_prime($p)) {
            $p += 2;
        }
        array_push($primes, $p);
    };

    while (count($primes) <= $number) {
        $add_next_prime();
    }

    return $primes[$number];
}
