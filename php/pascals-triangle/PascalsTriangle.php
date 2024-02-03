<?php

declare(strict_types=1);

class Factorial
{
    private $f = [1, 1, 2, 6];

    function factorial(int $n): int
    {
        for ($i = count($this->f) + 1; $i < $n; $i++) {
            array_push($this->f, end($this->f) * $i);
        }
        return $this->f[$n];
    }

    function choose(int $n, int $k): int
    {
        return $this->factorial($n) / $this->factorial($k) / $this->factorial($n - $k);
    }
}

function pascalsTriangleRows(int|null $rowCount): array|int
{
    if (is_null($rowCount) || $rowCount < 0) {
        return -1;
    }
    $fact = new Factorial();
    $triangle = [];

    for ($i = 0; $i < $rowCount; $i++) {
        $row = [];
        for ($j = 0; $j <= $i; $j++) {
            array_push($row, $fact->choose($i, $j));
        }
        array_push($triangle, $row);
    }
    return $triangle;
}
