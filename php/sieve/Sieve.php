<?php

declare(strict_types=1);

function sieve(int $limit): array
{
    if ($limit < 2) return [];

    $candidates = array_fill(2, $limit - 1, true);

    $mark_multiples = function ($p, $step) use (&$candidates, $limit) {
        for ($i = $p * $p; $i <= $limit; $i += $step) {
            $candidates[$i] = false;
        }
    };

    $mark_multiples(2, 2);
    for ($p = 3; $p * $p <= $limit; $p += 2) {
        if ($candidates[$p]) {
            $mark_multiples($p, 2 * $p);
        }
    }

    return array_keys(array_filter($candidates, fn($c) => $c));
}
