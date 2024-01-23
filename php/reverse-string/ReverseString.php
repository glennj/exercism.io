<?php

declare(strict_types=1);

function reverseString(string $text): string
{
    // The obvious cheat:
    // return strrev($text);

    // Iteration and swapping
    for ($i = 0, $j = strlen($text) - 1; $i < $j; $i++, $j--) {
        [$text[$i], $text[$j]] = [$text[$j], $text[$i]];
    }
    return $text;
}
