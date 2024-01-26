<?php

declare(strict_types=1);

function divmod(int $numerator, int $denominator): array
{
    return [intdiv($numerator, $denominator), $numerator % $denominator];
}

function floormod(int $numerator, int $denominator): int
{
    return (($numerator % $denominator) + $denominator) % $denominator;
}
