<?php

declare(strict_types=1);

const BOARD_SIZE = 8;

function placeQueen(int $x, int $y): bool
{
    $msg = '';
    if ($x < 0 || $y < 0) {
        $msg = 'The rank and file numbers must be positive.';
    } elseif ($x >= BOARD_SIZE || $y >= BOARD_SIZE) {
        $msg = 'The position must be on a standard size chess board.';
    }
    if ($msg) {
        throw new InvalidArgumentException($msg);
    }
    return true;
}

function canAttack(array $whiteQueen, array $blackQueen): bool
{
    [$bx, $by] = $blackQueen;
    placeQueen($bx, $by);
    [$wx, $wy] = $whiteQueen;
    placeQueen($wx, $wy);

    $dx = abs($bx - $wx);
    $dy = abs($by - $wy);
    return $dx == 0 || $dy == 0 || $dx == $dy;
}
