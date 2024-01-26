<?php

declare(strict_types=1);

require_once 'extra_array_funcs.php';

class Proverb
{
    public function recite(array $items): array
    {
        $lines = [];
        if (!empty($items)) {
            foreach (array_each_cons($items, 2) as [$x, $y]) {
                array_push($lines, "For want of a $x the $y was lost.");
            }
            array_push($lines, "And all for the want of a $items[0].");
        }
        return $lines;
    }
}
