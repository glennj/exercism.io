<?php

declare(strict_types=1);

require_once 'ResistorColor.php';

class ResistorColorDuo
{
    public function getColorsValue(array $colors): int
    {
        return 10 * colorCode($colors[0]) + colorCode($colors[1]);
    }
}
