<?php

declare(strict_types=1);

class Triangle
{
    private array $_sides;

    public function __construct(int|float ...$sides)
    {
        $this->_sides = $sides;
        sort($this->_sides);

        if ($this->_sides[0] <= 0
            || $this->_sides[0] + $this->_sides[1] <= $this->_sides[2]
        ) {
            throw new Exception("Not a triangle");
        }
    }

    public function kind(): string
    {
        switch (count(array_unique($this->_sides))) {
            case 1: return 'equilateral';
            case 2: return 'isosceles';
            default: return 'scalene';
        }
    }
}
