<?php

class Position
{
    public int $x;
    public int $y;

    function __construct(int $y, int $x) { // WTF (y, x) ??
        $this->x = $x;
        $this->y = $y;
    }
}
