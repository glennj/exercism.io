<?php

declare(strict_types=1);

require_once 'ResistorColor.php';
require_once 'ResistorColorDuo.php';

class ResistorColorTrio
{
    public function label(array $colors): string
    {
        $value = (new ResistorColorDuo())->getColorsValue(array_slice($colors, 0, 2));
        $value *= 10 ** colorCode($colors[2]);

        $idx = 0;
        if ($value > 0) {
            while ($value % 1000 == 0) {
                $value /= 1000;
                $idx += 1;
            }
        }
        $prefix = ['', 'kilo', 'mega', 'giga'][$idx];

        return "$value {$prefix}ohms";
    }
}
