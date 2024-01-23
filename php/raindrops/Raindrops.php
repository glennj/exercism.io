<?php

declare(strict_types=1);

function raindrops(int $number): string
{
    $drops = [3 => 'Pling', 5 => 'Plang', 7 => 'Plong'];
    $sounds = "";

    foreach ($drops as $val => $sound) {
        if ($number % $val == 0) {
            $sounds .= $sound;
        }
    }
    return $sounds ?: strval($number);
}
