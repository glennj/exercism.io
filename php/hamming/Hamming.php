<?php

declare(strict_types=1);

function distance(string $strandA, string $strandB): int
{
    if (strlen($strandA) != strlen($strandB)) {
        throw new InvalidArgumentException('DNA strands must be of equal length.');
    }

    return array_sum(
        array_map(
            fn($a, $b) => (int)($a != $b),
            str_split($strandA),
            str_split($strandB)
        )
    );
}
