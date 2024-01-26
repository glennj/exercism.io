<?php

declare(strict_types=1);
require_once 'extra_array_funcs.php';

function slices(string $digits, int $series): array
{
    if ($series < 1 || $series > strlen($digits)) {
        throw new Exception("Invalid series length");
    }
    $slices = [];
    foreach (array_each_cons(str_split($digits), $series) as $slice) {
        array_push($slices, implode($slice));
    }
    return $slices;
}
