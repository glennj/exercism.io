<?php

declare(strict_types=1);

function nextCollatz(int $n)
{
    return ($n % 2 == 0) ? $n / 2 : $n * 3 + 1;
}

// PHP doesn't do any tail-call optimizations
function stepsRecursive(int $number, int $steps = 0): int
{
    if ($number == 1) {
        return $steps;
    }
    return stepsRecursive(nextCollatz($number), $steps + 1);
}

function stepsIterative(int $number): int
{
    $steps = 0;
    while ($number > 1) {
        $number = nextCollatz($number);
        $steps++;
    }
    return $steps;
}

function steps(int $number): int
{
    if ($number < 1) {
        throw new InvalidArgumentException('Only positive numbers are allowed');
    }

    // return stepsRecursive($number);
    return stepsIterative($number);
}
