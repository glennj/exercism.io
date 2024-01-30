<?php

declare(strict_types=1);

function find(int $needle, array $haystack): int
{
    $i = 0;
    $j = count($haystack) - 1;
    while ($i <= $j) {
        $mid = intdiv($i + $j, 2);
        if ($needle == $haystack[$mid]) {
            return $mid;
        } elseif ($needle < $haystack[$mid]) {
            $j = $mid - 1;
        } else {
            $i = $mid + 1;
        }
    }
    return -1;
}
