<?php

declare(strict_types=1);

function difference(int $max): int
{
    return abs(sumOfSquares($max) - squareOfSum($max));
}

function squareOfSum(int $max): int
{
    return _sum($max, inner_fn: '_ident', outer_fn: '_sqr');
}

function sumOfSquares(int $max): int
{
    return _sum($max, inner_fn: '_sqr', outer_fn: '_ident');
}

// ----------------------------------------------------
function _ident(int $x): int
{
    return $x;
}

function _sqr(int $x): int
{
    return $x * $x;
}

function _sum(int $max, callable $inner_fn, callable $outer_fn): int
{
    return $outer_fn(array_sum(array_map($inner_fn, range(1, $max))));;
}
