<?php

declare(strict_types=1);

const A = 65;   // ord('A')

function diamond(string $letter): array
{
    $size = ord($letter) - A + 1;

    $create_row = function ($n) use ($size) {
        $right_half = array_fill(0, $size, ' ');
        $right_half[$n] = chr(A + $n);
        $left_half = array_reverse(array_slice($right_half, 1));
        return join(array_merge($left_half, $right_half));
    };

    $top_half = array_map($create_row, range(0, $size - 1));
    $bottom_half = array_slice(array_reverse($top_half), 1);

    return array_merge($top_half, $bottom_half);
}
